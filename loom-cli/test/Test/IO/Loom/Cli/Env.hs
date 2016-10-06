{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Cli.Env where

import           Loom.Cli.Env

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)

import           P

import           System.IO (IO)

import           Test.QuickCheck ((===), once)


prop_env_verify_good =
  once . testIO $ do
    e <- verifyExecutable "cat"
    pure $ e === Just "cat"

prop_env_verify_bad =
  once . testIO $ do
    e <- verifyExecutable "NOT_FOUND"
    pure $ e === Nothing


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
