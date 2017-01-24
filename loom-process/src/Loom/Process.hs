{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Loom.Process (
    ProcessError (..)
  , renderProcessError
  , call
  , call'
  , verifyExecutable
  ) where

import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadCatch, SomeException, handle, toException, try)
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

import           P

import           System.Directory (setCurrentDirectory)
import           System.Environment (getEnvironment)
import           System.Exit (ExitCode (..))
import           System.FilePath (FilePath)
import           System.IO (IO, stderr)
import qualified System.Process as Process

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT)


data Process =
  Process {
      processCommand :: FilePath
    , processArguments :: [Text]
    , processDirectory :: Maybe FilePath
    , processEnvironment :: Maybe (Map Text Text)
    } deriving (Eq, Ord, Show)


data ProcessError =
    ProcessFailure Process Int
  | ProcessException Process SomeException
  deriving (Show)


renderProcessError :: ProcessError -> Text
renderProcessError e =
  case e of
    ProcessFailure p code ->
      "Process failed: " <> T.intercalate " " ((T.pack . processCommand) p : processArguments p)
        <> " (exit code: " <> T.pack (show code) <> ")"

    ProcessException p ex ->
      "Process failed: " <> T.intercalate " " ((T.pack . processCommand) p : processArguments p)
        <> "\n" <> T.pack (show ex)

call :: (MonadIO m, MonadCatch m) => FilePath -> [Text] -> EitherT ProcessError m ()
call cmd args =
  callProcess
    Process {
      processCommand = cmd
    , processArguments = args
    , processDirectory = Nothing
    , processEnvironment = Nothing
    }

call' :: (MonadIO m, MonadCatch m) => Map Text Text -> FilePath -> [Text] -> EitherT ProcessError m ()
call' env cmd args =
  callProcess
    Process {
      processCommand = cmd
    , processArguments = args
    , processDirectory = Nothing
    , processEnvironment = Just env
    }

callProcess :: (MonadIO m, MonadCatch m) => Process -> EitherT ProcessError m ()
callProcess p@(Process cmd args d env) =
  handleIO p . newEitherT . liftIO $ do
    case d of
      Nothing ->
        return ()
      Just dir ->
        setCurrentDirectory dir
    env' <- case env of
      Nothing ->
        return Nothing
      Just env' ->
        fmap (Just . M.toList) $
          -- Concat the maps with the supplied values taking precedence
          (<>)
            <$> (pure . M.fromList . fmap (bimap T.unpack T.unpack) . M.toList $ env')
            -- Yes we're reading in the environment variables here, but it's purely to pass through
            <*> (M.fromList <$> getEnvironment)
    (_stdin, _stdout, _stderr, pid) <- Process.createProcess
      (Process.proc cmd (fmap T.unpack args)) {
        Process.env = env'
      , Process.std_out = Process.CreatePipe
      , Process.std_err = Process.UseHandle stderr
      }

    code <- Process.waitForProcess pid
    pure $ case code of
      ExitSuccess ->
        Right ()
      ExitFailure i ->
        Left $ ProcessFailure p i

handleIO :: MonadCatch m => Process -> EitherT ProcessError m a -> EitherT ProcessError m a
handleIO p =
  let
    fromIO = toException :: IOException -> SomeException
  in
     handle (hoistEither . Left . ProcessException p . fromIO)

verifyExecutable :: Text -> IO (Maybe FilePath)
verifyExecutable name = do
  x <- try $ do
    (_, _, _, pid) <- Process.createProcess $ (Process.proc (T.unpack name) []) {
        Process.std_in = Process.NoStream
      , Process.std_out = Process.NoStream
      , Process.std_err = Process.NoStream
      }
    Process.waitForProcess pid
  case x of
    Left (_ :: IOException) ->
      pure Nothing
    Right _ ->
      pure . Just . T.unpack $ name
