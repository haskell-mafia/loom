{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Loom.Build.Core (
    LoomBuildConfig
  , LoomMode (..)
  , LoomError (..)
  , LoomResult (..)
  , initialiseBuild
  , buildLoom
  , renderLoomBuildInitisationError
  , renderLoomError
  , machinatorOutputToProjector
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
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

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)

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
  deriving (Show)

initialiseBuild :: EitherT LoomBuildInitialiseError IO LoomBuildConfig
initialiseBuild =
  LoomBuildConfig
    <$> (newEitherT . fmap (maybeToRight LoomMissingSassExecutable)) Sass.findSassOnPath

buildLoom ::
  Logger (EitherT LoomError IO) ->
  LoomBuildConfig ->
  LoomMode ->
  LoomHome ->
  LoomTmp ->
  Loom ->
  EitherT LoomError IO LoomResult
buildLoom logger buildConfig mode home dir (Loom loomConfig' loomConfigs') = do
  resolved <- liftIO $
    LoomResolved
      <$> resolveLoom loomConfig'
      <*> mapM resolveLoom loomConfigs'
  buildLoomResolved logger buildConfig mode home dir resolved

resolveLoom :: LoomConfig -> IO LoomConfigResolved
resolveLoom config =
  LoomConfigResolved
    <$> (pure . loomConfigRoot) config
    <*> (pure . loomConfigName) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigComponents) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigSass) config
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigJsPaths) config
    <*> traverse (resolveBundle (loomConfigRoot config)) (loomConfigJsBundles config)
    <*> pure (loomConfigJsDepsNpm config)
    <*> pure (loomConfigJsDepsGithub config)
    <*> (fmap join . findFiles (loomConfigRoot config) . loomConfigPursPaths) config
    <*> pure (loomConfigPursDepsGithub config)

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

  po <- withLog logger "projector" $
    buildProjector components images mo

  purs <- withLog logger "purs" $
    buildPurescript home dir config configs components

  js <- withLog logger "js" $
    buildJs mode home dir config configs components purs

  pure $
    LoomResult
      (loomConfigResolvedName config)
      (Map.fromList (fmap (first loomConfigResolvedName) components))
      mo
      po
      outputCss
      images
      js

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
  -> Machinator.MachinatorOutput
  -> EitherT LoomError IO Projector.ProjectorOutput
buildProjector components images mo = do
  let
    pms = with components $ \(cr, cs) ->
      Projector.ProjectorInput
        (renderLoomName . loomConfigResolvedName $ cr)
        (loomRootFilePath . loomConfigResolvedRoot $ cr)
        images
        (bind (fmap componentFilePath . componentProjectorFiles) cs)
  firstT LoomProjectorError $
    foldMapM
      (Projector.compileProjector
        (Map.fromList .
          fmap (first (Projector.DataModuleName . Projector.ModuleName . Machinator.renderModuleName)) .
          Map.toList . Machinator.machinatorOutputDefinitions $ mo
          )
        )
      pms

buildPurescript ::
     LoomHome
  -> LoomTmp
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> EitherT LoomError IO (FilePath, Maybe Js.JsModuleName)
buildPurescript home dir config configs components =
  firstT LoomPursError $ do
    let
      psDepDir = Purescript.PurescriptUnpackDir (loomTmpFilePath dir </> "purs")
      psOutDir = Purescript.CodeGenDir (loomTmpFilePath dir </> "purs" </> "output")
      psOutFile = loomTmpFilePath dir </> "purs" </> "output" </> "out" <.> "js"
      psComponentFiles = fold (with components (foldMap componentPursFiles . snd))
      psPaths = foldMap loomConfigResolvedPurs configs -- FIXME this should probably include config too?
    psPathFiles <- fold <$> for psPaths (liftIO . Purescript.expandPursPath . loomFilePath)
    let
      psAll = psPathFiles <> fmap componentFilePath psComponentFiles
    deps <- Purescript.fetchPurs home (loomConfigResolvedPursDepsGithub config)
    Purescript.unpackPurs psDepDir deps
    mko <- Purescript.compile psDepDir psAll psOutDir
    res <- Purescript.bundlePurescript psOutDir mko
    liftIO $ T.writeFile psOutFile (Purescript.unJsBundle res)
    pure (psOutFile, Just (Js.JsModuleName "purs"))

buildJs ::
     LoomMode
  -> LoomHome
  -> LoomTmp
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> (FilePath, Maybe Js.JsModuleName)
  -> EitherT LoomError IO [JsFile]
buildJs mode home dir config configs components purs =
  firstT LoomJsError $ do
    -- Fetch and unpack dependencies
    deps <- Js.fetchJs home (loomConfigResolvedJsDepsNpm config) (loomConfigResolvedJsDepsGithub config)
    Js.unpackJs (jsDepDir dir) deps
    -- Produce each bundle and write out to disk.
    node <- Node.findNodeOnPath
    brow <- Browserify.installBrowserify home
    (:)
      <$> buildJsMain mode node brow dir config configs components purs
      <*> buildJsBundles mode node brow dir config configs components

outputJs :: LoomTmp -> FilePath -> JsFile
outputJs dir b =
  JsFile $ loomTmpFilePath dir </> b <.> "js"

jsDepDir :: LoomTmp -> Js.JsUnpackDir
jsDepDir dir =
  -- The "node_modules" component is mandatory - browserify expects to be able to require arbitrary modules.
  -- This involves walking backwards and peeking in every node_modules directory it can find.
  Js.JsUnpackDir (loomTmpFilePath dir </> "js" </> "node_modules")

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
    jsOut = outputJs dir "main"
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
  -> LoomConfigResolved
  -> [LoomConfigResolved]
  -> [(LoomConfigResolved, [Component])]
  -> EitherT JsError IO [JsFile]
buildJsBundles mode node brow dir config _configs components = do
  let
    bundleMap = Map.unionsWith (<>) $
        Map.fromList (loomConfigResolvedJsBundles config)
      : fmap Map.fromList (fmap (loomConfigResolvedJsBundles . fst) components)

  fmap Map.elems . flip Map.traverseWithKey bundleMap $ \bn (mains, paths) -> do
    let
      jsOut = outputJs dir (T.unpack (unBundleName bn))
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


machinatorOutputToProjector ::
  Machinator.MachinatorOutput ->
  Map Projector.DataModuleName [Machinator.Definition]
machinatorOutputToProjector =
  Map.fromList .
    fmap (first (Projector.DataModuleName . Projector.ModuleName . Machinator.renderModuleName)) .
    Map.toList . Machinator.machinatorOutputDefinitions

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

foldMapM :: (Foldable t, Monad m, Monoid b) => (b -> a -> m b) -> t a  -> m b
foldMapM f =
  foldM (\b -> fmap (mappend b) . f b) mempty
