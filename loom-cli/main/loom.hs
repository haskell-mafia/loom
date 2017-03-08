{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_loom_cli

import           Control.Monad.IO.Class (liftIO)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar

import qualified Data.List as List
import qualified Data.Text as T

import           DependencyInfo_ambiata_loom_cli


import           Language.Haskell.Interpreter (InterpreterT, OptionVal(..))
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Interpreter.Unsafe as Unsafe

import           Loom.Build.Core
import           Loom.Build.Data
import           Loom.Build.Haskell
import           Loom.Build.Logger
import           Loom.Build.Watch
import           Loom.Config.Toml
import           Loom.Http
import           Loom.Site

import qualified Network.Wai.Handler.Warp as Warp

import           P

import           System.Directory (getCurrentDirectory)
import           System.Environment (lookupEnv, setEnv)
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
  | Dev FilePath
  deriving (Eq, Show)

data LoomCliError =
    LoomError LoomError
  | LoomHaskellError LoomHaskellError
  | LoomSiteError LoomSiteError

-----------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  OA.cli "loom" buildInfoVersion dependencyInfo parser $ \c ->
    case c of
      Build -> do
        buildConfig <- orDie renderLoomBuildInitisationError initialiseBuild
        sitePrefix <- sitePrefixEnv
        cwd <- getCurrentDirectory
        config <- orDie renderLoomConfigTomlError $ resolveConfig cwd
        orDie renderLoomCliError $
          buildLoom'
            (newSimpleLogger stderr)
            buildConfig
            config
            sitePrefix
            (defaultLoomSiteRoot config)
      Watch port ->
        watch (const False) $ \config sitePrefix _ v -> do
          IO.hPutStrLn stderr $ "Starting loom at http://localhost:" <> show port
          let
            renderHtmlErrorPage' =
              renderHtmlErrorPage sitePrefix (loomConfigAssetsPrefix . loomConfig $ config)
          Warp.runSettings (Warp.setPort port Warp.defaultSettings) $
            loomHttpApplication
              (loomSiteRootFilePath . defaultLoomSiteRoot $ config)
              (LoomHttpNotFound . renderHtmlErrorPage' $ loomSiteNotFound)
              (LoomHttpBuild . fmap (first (renderHtmlErrorPage' . loomSiteError)) . MVar.readMVar $ v)
      Dev module' -> do
        -- TODO call as yet undefined mafia setup for doing everything _but_ compile the current project
        -- TODO Run separate warp server, intercept calls and either show build errors or the running server
        -- TODO calculate this correctly/dynamically
        setEnv "GHC_PACKAGE_PATH" "/Users/cofarrell/src/ambiata/ware/.cabal-sandbox/x86_64-apple-darwin/7.10.2/x86_64-osx-ghc-7.10.2-packages.conf.d:/Users/cofarrell/opt/ghc-7.10.2/lib/ghc-7.10.2/package.conf.d"
        -- TODO What should we watch? What about main?
        watch (List.isPrefixOf "src/") $ \_ _ x _ -> do
          r <- Hint.runInterpreter $ do
            go <- hint module' "main"
            void . forever $ do
              k <- go
              a <- liftIO . Async.async $ k
              _ <- liftIO x
              liftIO . Async.cancel $ a
            pure ()
          IO.print $ r

-----------

-- TODO Pass in loom and prefix
watch :: (FilePath -> Bool) -> (Loom -> LoomSitePrefix -> IO () -> MVar.MVar (Either Text ()) -> IO ()) -> IO ()
watch watchPred runner = do
  buildConfig <- orDie renderLoomBuildInitisationError initialiseBuild
  sitePrefix <- sitePrefixEnv
  cwd <- getCurrentDirectory
  config <- orDie renderLoomConfigTomlError $ resolveConfig cwd
  v <- MVar.newMVar $ Left "Building..."
  pin <- Pin.newPin
  -- TODO What's the best way to signal a file has changed to the caller?
  x <- MVar.newEmptyMVar
  watchA <- Async.async $
    let
      run = do
        MVar.modifyMVar_ v $ \_ -> do
          r <- runEitherT . firstT renderLoomCliError $
            buildLoom'
              (newSimpleLogger stderr)
              buildConfig
              config
              sitePrefix
              (defaultLoomSiteRoot config)
          IO.hPutStrLn stderr $ case r of
            Left e ->
              T.unpack e
            Right () ->
              "Build successful"
          pure r
        void $ MVar.tryPutMVar x ()
    in do
      -- Run once before watching files
      run
      watchTreeWithCancel pin cwd (\f -> loomWatchPredicate config f || watchPred f) $ \_file -> run
  runner config sitePrefix (MVar.takeMVar x) v
  Pin.pullPin pin
  Async.wait watchA

hint :: FilePath -> [Char] -> InterpreterT IO (InterpreterT IO (IO ()))
hint modul entrypoint = do
  Hint.set [Hint.searchPath := ["src", "dist/src"]]
  liftIO . IO.putStrLn $ "Loading " <> modul
  -- As best I can tell, this does a full reload, not an incremental reload
  -- This is unfortunate and possibly a dealbreaker
  -- might have to twiddle GHC API ourselves
  Hint.loadModules [modul]
  pure $ do
    Hint.reload
    Hint.setTopLevelModules ["Main"]
    Unsafe.unsafeInterpret entrypoint "IO ()"

-----------

buildLoom' ::
  Logger IO ->
  LoomBuildConfig ->
  Loom ->
  LoomSitePrefix ->
  LoomSiteRoot ->
  EitherT LoomCliError IO ()
buildLoom' logger buildConfig config sitePrefix siteRoot = do
  -- It's important to clean the site first so that subsequent requests will block until we have
  -- generated the new files
  liftIO $
    cleanLoomSite siteRoot
  firstT LoomSiteError $
    generateLoomSiteStatic siteRoot
  r <- firstT LoomError $
    buildLoom (hoistLogger liftIO logger) buildConfig (LoomTmp ".loom") config
  withLogIO logger "haskell" . firstT LoomHaskellError $
    -- NOTE: Site prefix is intentionally different for haskell than generated site
    generateHaskell (loomOutput config) (LoomSitePrefix "/") (loomConfigAssetsPrefix . loomConfig $ config) r
  withLogIO logger "site" . firstT LoomSiteError $
    generateLoomSite sitePrefix siteRoot (loomConfigAssetsPrefix . loomConfig $ config) r

-----------

sitePrefixEnv :: IO LoomSitePrefix
sitePrefixEnv =
  fmap (LoomSitePrefix . T.pack . fromMaybe "/") . lookupEnv $ "LOOM_SITE_PREFIX"

-----------

parser :: Parser Command
parser =
  OA.subparser . mconcat $ [
      OA.command' "build" "Build a loom project from the current working directory" $
        pure Build
    , OA.command' "watch" "Start an HTTP server in the current loom project and watch the filesystem for changes" $
        Watch <$> portP
    , OA.command' "dev" "Start a Haskell program and rebuild/reload on changes to relevant files" $
        Dev <$> moduleP
    ]

portP :: Parser Int
portP =
  fmap (fromMaybe 3000) . optional . OA.option (OA.eitherReader readEither) $
       OA.long "port"
    <> OA.short 'p'
    <> OA.metavar "PORT"
    <> OA.help "The HTTP port for running the server under. Defaults to port 3000."

moduleP :: Parser FilePath
moduleP =
  OA.option OA.str $
       OA.long "module"
    <> OA.short 'm'
    <> OA.metavar "MODULE"
    <> OA.help "The path to a haskell module to run"

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
