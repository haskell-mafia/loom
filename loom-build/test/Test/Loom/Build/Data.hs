{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Loom.Build.Data where

import           Loom.Build.Data

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.Loom.Build.Gen.Data
import qualified Test.QuickCheck.Jack as J

prop_build_file_pattern_parse =
  J.forAll genFilePattern $
    J.tripping renderFilePattern compileFilePattern

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
