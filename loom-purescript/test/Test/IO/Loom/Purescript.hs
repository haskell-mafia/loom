{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Purescript where

import qualified Data.Text.IO as T

import           Loom.Purescript

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll, neg)
import           Disorder.Core.IO (testIO)

import           P

import           System.FilePath ((</>))
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import qualified Test.QuickCheck as QC


prop_purescript_success =
  testIO $
    withTempDirectory "dist" "purescript" $ \dir -> do
      let
        f = dir </> "a.purs"
      T.writeFile f "module A where"
      r <- compilePurescript [f] dir
      pure $ checkPurescriptErrors r

prop_purescript_warnings =
  testIO $
    withTempDirectory "dist" "purescript" $ \dir -> do
      let
        f = dir </> "a.purs"
      T.writeFile f "module A where\n\nb = \"\""
      r <- compilePurescript [f] dir
      pure . neg $ checkPurescriptErrors r

prop_purescript_failure =
  testIO $
    withTempDirectory "dist" "purescript" $ \dir -> do
      let
        f = dir </> "a.purs"
      T.writeFile f "bad"
      r <- compilePurescript [f] dir
      pure . neg $ checkPurescriptErrors r

checkPurescriptErrors r =
  case r of
    Left errors ->
      QC.counterexample (show errors) False
    Right () ->
      QC.property True



return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
