{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Process where

import           Loom.Process

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.IO (IO)

import           Test.QuickCheck ((===), once)
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (runEitherT)

prop_process_echo args =
  once . testIO . testEitherT id $ do
    firstT renderProcessError $ call "echo" args
    pure True

prop_process_missing =
  once . testIO $ testEitherT id $ do
    m <- runEitherT $ call "badprocess" []
    pure $ isLeft m

prop_process_fail =
  once . testIO $ do
    m <- runEitherT $ call "cat" ["missing_file"]
    pure $ isLeft m

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
