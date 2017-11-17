{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_loom_cli

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar

import qualified Data.Text as T

import           DependencyInfo_ambiata_loom_cli

import           Loom.Build.Core
import           Loom.Build.Haskell
import           Loom.Build.Logger
import           Loom.Build.Purescript
import           Loom.Build.Watch
import           Loom.Core.Data
import           Loom.Config.Toml
import           Loom.Http
import qualified Loom.Js as Js
import qualified Loom.Js.Node as Node
import           Loom.Site

import qualified Network.Wai.Handler.Warp as Warp

import           P

import           System.Directory (getCurrentDirectory, getHomeDirectory)
import           System.Environment (lookupEnv)
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import           System.IO (BufferMode (..), FilePath, IO, hSetBuffering, stderr, stdout)
import qualified System.IO as IO

import qualified Twine.Data.Pin as Pin

import           X.Options.Applicative (Parser (..))
import qualified X.Options.Applicative as OA

import           X.Control.Monad.Trans.Either (EitherT, runEitherT)
import           X.Control.Monad.Trans.Either.Exit (orDie)

data Command =
    Build
  | Test
  | Watch Int [SiteFilter]
  | Paths PathsCommand
  deriving (Eq, Show)

data PathsCommand =
  PathsCommand {
      _pathsCommandBundle :: HardcodedBundle
    , _pathsCommandPathSet :: PathSet
    , _pathsCommandPathGrouping :: PathGrouping
    }
  deriving (Eq, Show)

data HardcodedBundle =
    Purs
  | PursTest
  deriving (Eq, Show)

data PathSet =
    LibPaths
  | AppPaths
  | AllPaths
  deriving (Eq, Show)

data PathGrouping =
    AsGlobs
  | AsFiles
  deriving (Eq, Show)

data LoomCliError =
    LoomError LoomError
  | LoomHaskellError LoomHaskellError
  | LoomPurescriptError LoomPurescriptError
  | LoomSiteError LoomSiteError

data BuildConfig =
  BuildConfig {
      _buildConfigHaskell :: FilePath
    , _buildConfigPurescript :: FilePath
    , buildConfigSite :: LoomSiteRoot
    , _buildGeneratePurescript :: Bool
    }

-----------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  OA.cli "loom" buildInfoVersion dependencyInfo parser $ \c ->
    case c of
      Build -> do
        buildConfig <- orDie renderLoomBuildInitisationError initialiseBuild
        home <- loomHomeEnv
        sitePrefix <- sitePrefixEnv
        apx <- assetsPrefixEnv
        bc <- buildConfigEnv
        cwd <- getCurrentDirectory
        config <- orDie renderLoomConfigTomlError $ resolveConfig cwd
        orDie renderLoomCliError $
          buildLoom'
            (newSimpleLogger stderr)
            buildConfig
            LoomProduction
            config
            home
            sitePrefix
            apx
            []
            bc
      Test -> do
        node <- orDie Js.renderJsError $
          Node.findNodeOnPath
        home <- loomHomeEnv
        cwd <- getCurrentDirectory
        config <- orDie renderLoomConfigTomlError $
          resolveConfig cwd
        loom <- resolveLoom config
        (nodePath, jsMain) <- orDie renderLoomError $
          buildTest
            (hoistLogger liftIO (newSimpleLogger stderr))
            home
            (LoomTmp ".loom")
            loom
        case jsMain of
          Nothing ->
            IO.hPutStrLn IO.stderr "No purs.test.main module specified"
          Just main' -> do
            ec <- Node.runNodeMain node nodePath main'
            Exit.exitWith ec
      Watch port sf ->
        watch port sf
      Paths (PathsCommand _ _ _) ->
        IO.hPutStrLn IO.stderr "TODO"

-----------

watch :: Int -> [SiteFilter] -> IO ()
watch port sf = do
  buildConfig <- orDie renderLoomBuildInitisationError initialiseBuild
  home <- loomHomeEnv
  sitePrefix <- sitePrefixEnv
  apx <- assetsPrefixEnv
  bc <- buildConfigEnv
  cwd <- getCurrentDirectory
  config <- orDie renderLoomConfigTomlError $ resolveConfig cwd
  v <- MVar.newMVar $ Left "Building..."
  pin <- Pin.newPin
  watchA <- Async.async $
    let
      run =
        MVar.modifyMVar_ v $ \_ -> do
          r <- runEitherT . firstT renderLoomCliError $
            buildLoom'
              (newSimpleLogger stderr)
              buildConfig
              LoomDevelopment
              config
              home
              sitePrefix
              apx
              sf
              bc
          IO.hPutStrLn stderr $ case r of
            Left e ->
              T.unpack e
            Right () ->
              "Build successful"
          pure r
    in do
      -- Run once before watching files
      run
      loomWatch pin cwd config $ \_file -> run
  IO.hPutStrLn stderr $ "Starting loom at http://localhost:" <> show port
  let
    renderHtmlErrorPage' =
      renderHtmlErrorPage sitePrefix
  w <- Async.async .
    Warp.runSettings (Warp.setPort port Warp.defaultSettings) $
      loomHttpApplication
        (loomSiteRootFilePath . buildConfigSite $ bc)
        (LoomHttpNotFound . renderHtmlErrorPage' $ loomSiteNotFound)
        (LoomHttpBuild . fmap (first (renderHtmlErrorPage' . loomSiteError)) . MVar.readMVar $ v)
  x <- Async.waitEither watchA w
  case x of
    Left _ ->
      Async.cancel w
    Right _ -> do
      Pin.pullPin pin
      Async.wait watchA

-----------

buildLoom' ::
  Logger IO ->
  LoomBuildConfig ->
  LoomMode ->
  Loom ->
  LoomHome ->
  LoomSitePrefix ->
  AssetsPrefix ->
  [SiteFilter] ->
  BuildConfig ->
  EitherT LoomCliError IO ()
buildLoom' logger buildConfig mode config home sitePrefix apx sf (BuildConfig haskellRoot pursRoot siteRoot genPurs) = do
  -- It's important to clean the site first so that subsequent requests will block until we have
  -- generated the new files
  liftIO $
    cleanLoomSite siteRoot
  firstT LoomSiteError $
    generateLoomSiteStatic sitePrefix siteRoot
  r <- firstT LoomError $
    buildLoom (hoistLogger liftIO logger) buildConfig mode home (LoomTmp ".loom") config
  withLogIO logger "haskell" . firstT LoomHaskellError $
    -- NOTE: Site prefix is intentionally different for haskell than generated site
    generateHaskell haskellRoot (LoomSitePrefix "/") apx r
  when genPurs $
    withLogIO logger "purescript" . firstT LoomPurescriptError $
      generatePurescript pursRoot (LoomSitePrefix "/") apx r
  withLogIO logger "site" . firstT LoomSiteError $
    generateLoomSite sitePrefix siteRoot apx sf r

-----------

loomHomeEnv :: IO LoomHome
loomHomeEnv = do
  home <- getHomeDirectory
  fmap (LoomHome . fromMaybe (home </> ".loom")) . lookupEnv $ "LOOM_HOME"

sitePrefixEnv :: IO LoomSitePrefix
sitePrefixEnv =
  fmap (LoomSitePrefix . T.pack . fromMaybe "/") . lookupEnv $ "LOOM_SITE_PREFIX"

assetsPrefixEnv :: IO AssetsPrefix
assetsPrefixEnv =
  fmap (AssetsPrefix . fromMaybe "assets") . lookupEnv $ "LOOM_ASSETS_PREFIX"

buildConfigEnv :: IO BuildConfig
buildConfigEnv =
  BuildConfig
    <$> (fmap (fromMaybe "dist/loom/haskell") . lookupEnv) "LOOM_OUTPUT_HASKELL"
    <*> (fmap (fromMaybe "dist/loom/purs") . lookupEnv) "LOOM_OUTPUT_PURESCRIPT"
    <*> (fmap (LoomSiteRoot . fromMaybe "dist/loom/site") . lookupEnv) "LOOM_OUTPUT_SITE"
    <*> (fmap (maybe False (const True)) (lookupEnv "LOOM_GENERATE_PURESCRIPT"))

-----------

parser :: Parser Command
parser =
  OA.subparser . mconcat $ [
      OA.command' "build" "Build a loom project from the current working directory" $
        pure Build
    , OA.command' "test" "Test a loom project from the current working directory" $
        pure Test
    , OA.command' "watch" "Start an HTTP server in the current loom project and watch the filesystem for changes" $
        Watch <$> portP <*> many siteFilterP
    , OA.command' "paths" "Dump out paths for purs, sass and other file groups." $
        Paths <$> pathsP
    ]

portP :: Parser Int
portP =
  fmap (fromMaybe 3000) . optional . OA.option (OA.eitherReader readEither) $
       OA.long "port"
    <> OA.short 'p'
    <> OA.metavar "PORT"
    <> OA.help "The HTTP port for running the server under. Defaults to port 3000."

siteFilterP :: Parser SiteFilter
siteFilterP =
  fmap SiteFilter . OA.option (OA.eitherReader (first T.unpack . compileFilePattern . T.pack)) $
       OA.long "site-filter"
    <> OA.metavar "SITE_FILTER"
    <> OA.help "A filter for generating a sub-set of the site to reduce the time taken"

pathsP :: Parser PathsCommand
pathsP =
  PathsCommand
    <$> OA.argument
          (OA.eitherTextReader id parseBundle)
          (OA.metavar "BUNDLE")
    <*> pathSetP
    <*> pathGroupingP

parseBundle :: Text -> Either Text HardcodedBundle
parseBundle = \case
  "purs" ->
    pure Purs
  "purs.test" ->
    pure PursTest
  _ ->
    Left "Unsupported bundle; only available for 'purs' and 'purs.test'"

pathSetP :: Parser PathSet
pathSetP =
      OA.flag' AllPaths (
           OA.long "all-paths"
        <> OA.help "All Paths (default)"
        )
  <|> OA.flag' LibPaths (
           OA.long "lib-paths"
        <> OA.help "Library Paths"
        )
  <|> OA.flag' AppPaths (
           OA.long "app-paths"
        <> OA.help "App Paths"
        )
  <|> pure AllPaths

pathGroupingP :: Parser PathGrouping
pathGroupingP =
      OA.flag' AsGlobs (
           OA.long "as-globs"
        <> OA.help "Display as path globs (default)"
        )
  <|> OA.flag' AsFiles (
           OA.long "as-files"
        <> OA.help "Display the full file list"
        )
  <|> pure AsGlobs


-----------

renderLoomCliError :: LoomCliError -> Text
renderLoomCliError le =
  case le of
    LoomError e ->
      renderLoomError e
    LoomHaskellError e ->
      renderLoomHaskellError e
    LoomPurescriptError e ->
      renderLoomPurescriptError e
    LoomSiteError e ->
      renderLoomSiteError e
