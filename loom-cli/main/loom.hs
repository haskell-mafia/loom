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
import           Loom.Build.Watch
import           Loom.Core.Data
import           Loom.Config.Toml
import qualified Loom.Fetch as Fetch
import           Loom.Fetch.HTTPS (HTTPSError, renderHTTPSError)
import qualified Loom.Fetch.HTTPS.Github as FG
import qualified Loom.Fetch.HTTPS.Npm as FN
import           Loom.Http
import           Loom.Site

import qualified Network.Wai.Handler.Warp as Warp

import           P

import           System.Directory (getCurrentDirectory, getHomeDirectory)
import           System.Environment (lookupEnv)
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
  | Watch Int
  | Fetch Source Text Text
  deriving (Eq, Show)

data Source =
    Github
  | Npm
  deriving (Eq, Show)

data LoomCliError =
    LoomError LoomError
  | LoomHaskellError LoomHaskellError
  | LoomSiteError LoomSiteError
  | LoomFetchError [Fetch.FetchError HTTPSError]

data BuildConfig =
  BuildConfig {
      _buildConfigHaskell :: FilePath
    , buildConfigSite :: LoomSiteRoot
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
            bc
      Watch port ->
        watch port
      Fetch src n v ->
        orDie renderLoomCliError $
          fetch src n v

-----------

watch :: Int -> IO ()
watch port = do
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
  Warp.runSettings (Warp.setPort port Warp.defaultSettings) $
    loomHttpApplication
      (loomSiteRootFilePath . buildConfigSite $ bc)
      (LoomHttpNotFound . renderHtmlErrorPage' $ loomSiteNotFound)
      (LoomHttpBuild . fmap (first (renderHtmlErrorPage' . loomSiteError)) . MVar.readMVar $ v)
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
  BuildConfig ->
  EitherT LoomCliError IO ()
buildLoom' logger buildConfig mode config home sitePrefix apx (BuildConfig haskellRoot siteRoot) = do
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
  withLogIO logger "site" . firstT LoomSiteError $
    generateLoomSite sitePrefix siteRoot apx r

-----------

fetch :: Source -> Text -> Text -> EitherT LoomCliError IO ()
fetch source n v = do
  home <- liftIO loomHomeEnv
  firstT LoomFetchError $ case source of
    Npm -> do
      npm <- liftIO FN.npmFetcher
      let npmNamer = const (T.unpack n)
      res <- Fetch.fetchDeps
        home
        npm
        npmNamer
        [NpmDependency (NpmPackage n) (NpmPackageVersion v) (Sha1 "")]
      liftIO . for_ res $ printFetched
    Github -> do
      gh <- liftIO FG.githubFetcher
      let -- FIXME partial
          [user, repo] = T.splitOn "/" n
          ghNamer = const (T.unpack repo)
      res <- Fetch.fetchDeps
        home
        gh
        ghNamer
        [GithubDependency (GithubRepo user repo) (GitRef v) (Sha1 "")]
      liftIO . for_ res $ printFetched

printFetched :: Fetch.FetchedDependency -> IO ()
printFetched =
  \(Fetch.FetchedDependency name (Tarball _file) (Sha1 hash)) ->
    liftIO . IO.putStrLn $ name <> " " <> T.unpack hash

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
    <*> (fmap (LoomSiteRoot . fromMaybe "dist/loom/site") . lookupEnv) "LOOM_OUTPUT_SITE"

-----------

parser :: Parser Command
parser =
  OA.subparser . mconcat $ [
      OA.command' "build" "Build a loom project from the current working directory" $
        pure Build
    , OA.command' "watch" "Start an HTTP server in the current loom project and watch the filesystem for changes" $
        Watch <$> portP
    , OA.command' "fetch" "Fetch, cache, and hash a remote dependency" $
        Fetch <$> fetchSourceP <*> fetchNameP <*> fetchVersionP
    ]

portP :: Parser Int
portP =
  fmap (fromMaybe 3000) . optional . OA.option (OA.eitherReader readEither) $
       OA.long "port"
    <> OA.short 'p'
    <> OA.metavar "PORT"
    <> OA.help "The HTTP port for running the server under. Defaults to port 3000."

fetchSourceP :: Parser Source
fetchSourceP =
  asum [
      OA.flag' Npm $
           OA.long "npm"
        <> OA.short 'n'
    , OA.flag' Github $
           OA.long "github"
        <> OA.short 'g'
    ]

fetchNameP :: Parser Text
fetchNameP =
  fmap T.pack . OA.strArgument $
    OA.metavar "DEPENDENCY_NAME"

fetchVersionP :: Parser Text
fetchVersionP =
  fmap T.pack . OA.strArgument $
    OA.metavar "DEPENDENCY_VERSION"

-----------

renderLoomCliError :: LoomCliError -> Text
renderLoomCliError le =
  case le of
    LoomError e ->
      renderLoomError e
    LoomHaskellError e ->
      renderLoomHaskellError e
    LoomSiteError e ->
      renderLoomSiteError e
    LoomFetchError es ->
      T.unlines $ fmap (Fetch.renderFetchError renderHTTPSError) es
