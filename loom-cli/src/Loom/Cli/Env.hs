{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Loom.Cli.Env (
    verifyExecutable
  ) where

import           Control.Exception (IOException)
import           Control.Monad.Catch (try)

import qualified Data.Text as T

import           Loom.Cli.File

import           P

import           System.IO (IO)
import qualified System.Process as Process


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
      pure $ Just name
