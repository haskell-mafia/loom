{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Loom.Build.Core (
    LoomBuildConfig
  , LoomMode (..)
  , LoomError (..)
  , LoomResult (..)
  , initialiseBuild
  , resolveLoom
  , buildLoom
  , buildTest
  , renderLoomBuildInitisationError
  , renderLoomError
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
#if MIN_VERSION_containers(0, 5, 9)
import qualified Data.Map.Merge.Strict as Map
#else
import qualified Data.Map.Strict.Merge as Map
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Build.Component
import           Loom.Build.Data
import           Loom.Build.Logger
import           Loom.Core.Data
import           Loom.Js (JsError)
import qualified Loom.Js as Js
import qualified Loom.Js.Node as Node
import qualified Loom.Js.Browserify as Browserify
import           Loom.Machinator (MachinatorInput (..), MachinatorError)
import qualified Loom.Machinator as Machinator
import           Loom.Projector (ProjectorError)
import qualified Loom.Projector as Projector
import           Loom.Purescript (PurescriptError)
import qualified Loom.Purescript as Purescript
import           Loom.Sass (Sass, SassError)
import qualified Loom.Sass as Sass

import           P

import           System.FilePath ((</>), (<.>))
import           System.IO (IO, FilePath)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT)

-- | Can improve the performance of the build, and generate useful developer artifacts
-- Please handle with care - we _never_ want to miss bugs in developer mode
data LoomMode =
    LoomDevelopment
  | LoomProduction

data LoomBuildConfig =
  LoomBuildConfig Sass

data LoomBuildInitialiseError =
    LoomMissingSassExecutable
  deriving (Show)

data LoomError =
    LoomSassError SassError
  | LoomComponentError ComponentError
  | LoomProjectorError ProjectorError
  | LoomMachinatorError MachinatorError
  | LoomJsError JsError
  | LoomPursError PurescriptError
  | LoomDependencyMismatch Text Sha1 Sha1
  deriving (Show)

initialiseBuild :: EitherT LoomBuildInitialiseError IO LoomBuildConfig
initialiseBuild =
  LoomBuildConfig
    <$> (newEitherT . fmap (maybeToRight LoomMissingSassExecutable)) Sass.findSassOnPath

resolveLoom :: Loom -> IO LoomResolved
resolveLoom (Loom loomConfig' loomConfigs') =
  LoomResolved
    <$> resolveLoomConfig loomConfig'
    <*> mapM resolveLoomConfig loomConfigs'

buildLoom ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomMode ->
  LoomHome ->
  LoomTmp ->
  Loom ->
  EitherT LoomError IO LoomResult
buildLoom logger buildConfig mode home dir loom = do
  resolved <- liftIO $
    resolveLoom loom
  buildLoomResolved logger buildConfig mode home dir resolved

resolveLoomConfig :: LoomConfig -> IO LoomConfigResolved
resolveLoomConfig config =
  LoomConfigResolved
    <$> (pure . loomConfigRoot) config
    <*> (pure . loomConfigName) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigComponents) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigSass) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigJsPaths) config
    <*> traverse (resolveBundle (loomConfigRoot config)) (loomConfigJsBundles config)
    <*> pure (loomConfigJsDepsNpm config)
    <*> pure (loomConfigJsDepsGithub config)
    <*> (resolvePurescriptBundle (loomConfigRoot config) . loomConfigPurs) config
    <*> (resolvePurescriptBundle (loomConfigRoot config) . loomConfigPursTest) config

resolvePurescriptBundle :: LoomRoot -> PurescriptBundle FilePattern -> IO (PurescriptBundle LoomFile)
resolvePurescriptBundle root b =
  PurescriptBundle
    <$> (fmap join . findFiles root . purescriptBundleFiles) b
    <*> (pure . purescriptBundleDependencies) b
    <*> (pure . purescriptBundleMain) b

resolveBundle :: LoomRoot -> Bundle -> IO (BundleName, ([LoomFile], [LoomFile]))
resolveBundle root (Bundle bn main others) = do
  m <-  fmap join (findFiles root [main])
  os <- fmap join (findFiles root others)
  pure (bn, (m, os))

-- FIX This function currently makes _no_ attempt at caching results. Yet
buildLoomResolved ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomMode ->
  LoomHome ->
  LoomTmp ->
  LoomResolved ->
  EitherT LoomError IO LoomResult
buildLoomResolved logger (LoomBuildConfig sass) mode home dir (LoomResolved config others) = do
  let
    configs =
      -- Need to make sure the dependencies are in reverse order
      reverse $ config : others
  components <- firstT LoomComponentError . for configs $ \c ->
    fmap ((,) c) . resolveComponents . loomConfigResolvedComponents $ c

  --- Images ---
  let
    images = components >>= \(cr, cs) ->
      bind (\c -> fmap (ImageFile (loomConfigResolvedName cr)) . componentImageFiles $ c) cs

  outputCss <- withLog logger "sass" $
    buildCss sass dir config configs components

  mo <- withLog logger "machinator" $
    buildMachinator components

  let
    bundleMap = mkBundleMap config components
    bundles =
        (mainBundle, bundleOut dir mainBundle)
      : with (Map.keys bundleMap) (\bn -> (bn, bundleOut dir bn))

  po <- withLog logger "projector" $
    buildProjector components images bundles mo

  (_, psOut, psMod) <- withLog logger "purs" $
    buildPurescript home dir config others components

  _js <- withLog logger "js" $
    buildJs mode home dir config configs components (psOut, psMod) bundleMap

  pure $
    LoomResult
      (loomConfigResolvedName config)
      (Map.fromList (fmap (first loomConfigResolvedName) components))
      mo
      po
      outputCss
      images
      bundles

buildTest ::
  Logger (EitherT LoomError IO) ->
  LoomHome ->
  LoomTmp ->
  LoomResolved ->
  EitherT LoomError IO (Node.NodePath, Maybe Js.JsModuleName)
buildTest logger home dir (LoomResolved config others) = do
  let
    configs =
      -- Need to make sure the dependencies are in reverse order
      reverse $ config : others
  components <- firstT LoomComponentError . for configs $ \c ->
    resolveComponents . loomConfigResolvedComponents $ c

  withLog logger "js" $ do
    (npm, gh) <- hoistEither $
      resolveJsDependencies config configs
    firstT LoomJsError $ do
      deps <- Js.fetchJs home npm gh
      Js.unpackJs (jsDepDir dir) deps

  PurescriptBundle psAll deps main <- resolvePurescript
    (loomConfigResolvedPursTest config)
    (fmap loomConfigResolvedPursTest others <> fmap loomConfigResolvedPurs configs)
    components

  psDir <- withLog logger "purs" $
    firstT LoomPursError $ do
      let
        psDir = loomTmpFilePath dir </> "purs" </> "test"
        psDepDir = Purescript.PurescriptUnpackDir psDir
        psOutDir = Purescript.CodeGenDir (psDir </> "output")
      fetchedDeps <- Purescript.fetchPurs home deps
      Purescript.unpackPurs psDepDir fetchedDeps
      (_ :: Purescript.MakeOutput) <- Purescript.compile psDepDir psAll psOutDir
      pure psOutDir

  pure $ (,)
    (Node.NodePath $ [
        (Js.unJsUnpackDir . jsDepDir) dir
      , Purescript.unCodeGenDir psDir
      ] <> (fmap loomFilePath . bind loomConfigResolvedJs) configs
      )
    (fmap Js.JsModuleName main)

buildCss ::
     Sass
  -> LoomTmp
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> EitherT LoomError IO CssFile
buildCss sass dir config configs components = do
  let
    outputCss = CssFile $ loomTmpFilePath dir </> (T.unpack . renderLoomName . loomConfigResolvedName) config <> ".css"
    inputs =
      mconcat . mconcat $ [
          fmap (\c -> fmap loomFilePath . loomConfigResolvedSass $ c) configs
        , fmap (fmap componentFilePath . componentSassFiles) . bind snd $ components
        ]
  firstT LoomSassError $
    Sass.compileSass sass Sass.SassCompressed outputCss inputs
  pure outputCss

buildMachinator :: [(LoomConfigResolved, [Component])] -> EitherT LoomError IO Machinator.MachinatorOutput
buildMachinator components = do
  let
    mms = with components $ \(cr, cs) ->
      MachinatorInput
        (Machinator.ModuleName . renderLoomName . loomConfigResolvedName $ cr)
        (loomRootFilePath . loomConfigResolvedRoot $ cr)
        (bind (fmap componentFilePath . componentMachinatorFiles) cs)
  firstT LoomMachinatorError $
    Machinator.compileMachinator mms

buildProjector ::
     [(LoomConfigResolved, [Component])]
  -> [ImageFile]
  -> [(BundleName, JsFile)]
  -> Machinator.MachinatorOutput
  -> EitherT LoomError IO Projector.ProjectorOutput
buildProjector components images js (Machinator.MachinatorOutput mo) = do
  let
    pms = with components $ \(cr, cs) ->
      Projector.ProjectorInput
        (renderLoomName . loomConfigResolvedName $ cr)
        (loomRootFilePath . loomConfigResolvedRoot $ cr)
        images
        js
        (bind (fmap componentFilePath . componentProjectorFiles) cs)
  firstT LoomProjectorError $
    foldMapM
      (Projector.compileProjector mo)
      pms

mkBundleMap ::
     LoomConfigResolved
  -> [(LoomConfigResolved, [Component])]
  -> Map BundleName ([LoomFile], [LoomFile])
mkBundleMap config components =
  Map.unionsWith (<>) $
      Map.fromList (loomConfigResolvedJsBundles config)
    : fmap Map.fromList (fmap (loomConfigResolvedJsBundles . fst) components)

mainBundle :: BundleName
mainBundle =
  BundleName "main"

buildPurescript ::
     LoomHome
  -> LoomTmp
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> EitherT LoomError IO (Purescript.CodeGenDir, FilePath, Maybe Js.JsModuleName)
buildPurescript home dir config others components = do
  bundle <- resolvePurescript
    (loomConfigResolvedPurs config) (fmap loomConfigResolvedPurs others) (fmap snd components)
  firstT LoomPursError $
    buildPurescript' home dir bundle

resolvePurescript ::
     PurescriptBundle LoomFile
  -> [PurescriptBundle LoomFile]
  -> [[Component]]
  -> EitherT LoomError IO (PurescriptBundle FilePath)
resolvePurescript config others components = do
  let
    configs = config : others
  deps <- hoistEither . resolveGithubDeps . fmap purescriptBundleDependencies $
    configs
  let
    psComponentFiles = fold (with components (foldMap componentPursFiles))
    psPaths = foldMap purescriptBundleFiles configs
  psPathFiles <- fold <$> for psPaths (liftIO . Purescript.expandPursPath . loomFilePath)
  pure $
    PurescriptBundle
      (psPathFiles <> fmap componentFilePath psComponentFiles)
      deps
      (purescriptBundleMain config)

buildPurescript' ::
     LoomHome
  -> LoomTmp
  -> PurescriptBundle FilePath
  -> EitherT PurescriptError IO (Purescript.CodeGenDir, FilePath, Maybe Js.JsModuleName)
buildPurescript' home dir (PurescriptBundle psAll deps _) = do
  let
    psDir = loomTmpFilePath dir </> "purs"
    psDepDir = Purescript.PurescriptUnpackDir psDir
    psOutDir = Purescript.CodeGenDir (psDir </> "output")
    psOutFile = psDir </> "output" </> "out" <.> "js"
  fetchedDeps <- Purescript.fetchPurs home deps
  Purescript.unpackPurs psDepDir fetchedDeps
  mko <- Purescript.compile psDepDir psAll psOutDir
  res <- Purescript.bundlePurescript psOutDir mko
  liftIO $ T.writeFile psOutFile (Purescript.unJsBundle res)
  pure (psOutDir, psOutFile, Just (Js.JsModuleName "purs"))

resolveGithubDeps :: [[GithubDependency]] -> Either LoomError [GithubDependency]
resolveGithubDeps allDeps =
  let mkMap :: [GithubDependency] -> Map Text GithubDependency
      mkMap = Map.fromList . fmap (\g@(GithubDependency repo _ref _sha) -> (grRepo repo, g))
      check k g1 g2 =
        if g1 == g2
          then pure g1
          else Left (LoomDependencyMismatch k (ghdSha1 g1) (ghdSha1 g2))
  in fmap Map.elems (unionsWithM check (fmap mkMap allDeps))

resolveNpmDeps :: [[NpmDependency]] -> Either LoomError [NpmDependency]
resolveNpmDeps allDeps =
  let mkMap :: [NpmDependency] -> Map Text NpmDependency
      mkMap = Map.fromList . fmap (\n@(NpmDependency pack _ver _sha) -> (unNpmPackage pack, n))
      check k n1 n2 =
        if n1 == n2
          then pure n1
          else Left (LoomDependencyMismatch k (ndSha1 n1) (ndSha1 n2))
  in fmap Map.elems (unionsWithM check (fmap mkMap allDeps))

unionsWithM :: (Monad m, Foldable f, Ord k) => (k -> a -> a -> m a) -> f (Map k a) -> m (Map k a)
unionsWithM k =
  foldM
    (Map.mergeA Map.preserveMissing Map.preserveMissing (Map.zipWithAMatched k))
    mempty

buildJs ::
     LoomMode
  -> LoomHome
  -> LoomTmp
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> (FilePath, Maybe Js.JsModuleName)
  -> Map BundleName ([LoomFile], [LoomFile])
  -> EitherT LoomError IO [JsFile]
buildJs mode home dir config configs components purs bundleMap = do
  (npm, gh) <- hoistEither (resolveJsDependencies config configs)
  firstT LoomJsError $ do
    -- Fetch and unpack dependencies
    deps <- Js.fetchJs home npm gh
    Js.unpackJs (jsDepDir dir) deps
    -- Produce each bundle and write out to disk.
    node <- Node.findNodeOnPath
    brow <- Browserify.installBrowserify home
    (:)
      <$> buildJsMain mode node brow dir config configs components purs
      <*> buildJsBundles mode node brow dir bundleMap

outputJs :: LoomTmp -> FilePath -> JsFile
outputJs dir b =
  JsFile $ loomTmpFilePath dir </> b <.> "js"

jsDepDir :: LoomTmp -> Js.JsUnpackDir
jsDepDir dir =
  -- The "node_modules" component is mandatory - browserify expects to be able to require arbitrary modules.
  -- This involves walking backwards and peeking in every node_modules directory it can find.
  Js.JsUnpackDir (loomTmpFilePath dir </> "js" </> "node_modules")

resolveJsDependencies :: LoomConfigResolved -> [LoomConfigResolved] -> Either LoomError ([NpmDependency], [GithubDependency])
resolveJsDependencies config configs =
  let ghDeps = loomConfigResolvedJsDepsGithub config : fmap loomConfigResolvedJsDepsGithub configs
      npmDeps = loomConfigResolvedJsDepsNpm config : fmap loomConfigResolvedJsDepsNpm configs
  in (,) <$> resolveNpmDeps npmDeps <*> resolveGithubDeps ghDeps

bundleOut :: LoomTmp -> BundleName -> JsFile
bundleOut dir =
  outputJs dir . T.unpack . unBundleName

-- Special case for the 'main' bundle.
buildJsMain ::
     LoomMode
  -> Node.Node
  -> Browserify.Browserify
  -> LoomTmp
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> (FilePath, Maybe Js.JsModuleName)
  -> EitherT JsError IO JsFile
buildJsMain mode node brow dir _config configs components purs = do
  let
    jsOut = bundleOut dir (BundleName "main")
              -- FIXME should be adding paths from config here
    jsPaths = foldMap (fmap (Js.JsUnpackDir . loomFilePath) . loomConfigResolvedJs) configs
    jsComponentEntries =
      fold . with components $ \(cr, cs) ->
        with (foldMap componentJsFiles cs) $ \cf@(ComponentFile (LoomFile _ path) _) ->
          let relative = "." </> componentFilePath cf -- The "." is necessary for browserify, it can't path
              prefixed = Js.JsModuleName (renderLoomName (loomConfigResolvedName cr) <> "/" <> T.pack path)
          in -- e.g. ("./modules/confirm-button/vanilla.js", "bikeshed/modules/general/confirm-button")
             (relative, Just prefixed)
    binput = Browserify.BrowserifyInput {
       Browserify.browserifyMode =
         case mode of
           LoomDevelopment ->
             Browserify.BrowserifyDev
           LoomProduction ->
             Browserify.BrowserifyProd
     , Browserify.browserifyPaths = jsDepDir dir : jsPaths
     , Browserify.browserifyEntries =
          purs
        : jsComponentEntries
     }
  reso <- Browserify.runBrowserify node brow binput
  liftIO (T.writeFile (renderJsFile jsOut) (Browserify.unBrowserifyOutput reso))
  pure jsOut

-- All the other bundles.
buildJsBundles ::
     LoomMode
  -> Node.Node
  -> Browserify.Browserify
  -> LoomTmp
  -> Map BundleName ([LoomFile], [LoomFile])
  -> EitherT JsError IO [JsFile]
buildJsBundles mode node brow dir bundleMap = do
  fmap Map.elems . flip Map.traverseWithKey bundleMap $ \bn (mains, paths) -> do
    let
      jsOut = bundleOut dir bn
      jsPaths = fmap (Js.JsUnpackDir . loomFilePath) paths
      binput = Browserify.BrowserifyInput {
          Browserify.browserifyMode =
            case mode of
              LoomDevelopment ->
                Browserify.BrowserifyDev
              LoomProduction ->
                Browserify.BrowserifyProd
        , Browserify.browserifyPaths = jsDepDir dir : jsPaths
        , Browserify.browserifyEntries =
            with mains (\lf -> ("." </> loomFilePath lf, Nothing))
        }
    reso <- Browserify.runBrowserify node brow binput
    liftIO (T.writeFile (renderJsFile jsOut) (Browserify.unBrowserifyOutput reso))
    pure jsOut

renderLoomBuildInitisationError :: LoomBuildInitialiseError -> Text
renderLoomBuildInitisationError ie =
  case ie of
    LoomMissingSassExecutable ->
      T.unlines [
          "Could not locate 'sassc' executable on the PATH."
        , ""
        , "For OSX users try running the following first:"
        , " - brew install sassc"
        , ""
        , "For Arch users try running the following first:"
        , " - pacman -S sassc"
        , ""
        , "Otherwise please follow the build instructions here:"
        , " - https://github.com/sass/sassc#documentation"
        ]

renderLoomError :: LoomError -> Text
renderLoomError le =
  case le of
    LoomSassError se ->
      Sass.renderSassError se
    LoomComponentError e ->
      renderComponentError e
    LoomProjectorError e ->
      Projector.renderProjectorError e
    LoomMachinatorError e ->
      Machinator.renderMachinatorError e
    LoomJsError e ->
      Js.renderJsError e
    LoomPursError e ->
      Purescript.renderPurescriptError e
    LoomDependencyMismatch k (Sha1 h1) (Sha1 h2) ->
      T.unlines [
          "Dependency mismatch for '" <> k <> "'"
        , "  " <> h1 <> " /= " <> h2
        , "  (At present, all loom projects must have consistent external dependencies.)"
        ]

foldMapM :: (Foldable t, Monad m, Monoid b) => (b -> a -> m b) -> t a  -> m b
foldMapM f =
  foldM (\b -> fmap (mappend b) . f b) mempty
