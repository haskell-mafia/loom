{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Purescript where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text.IO as T

import           Loom.Purescript

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll, neg)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.FilePath ((</>))
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import qualified Test.QuickCheck as QC


prop_purescript_success =
  QC.once . testIO . withTempDirectory "dist" "purescript" $ \dir ->
    withTempDirectory "dist" "purescript" $ \tmp ->
      testEitherT renderPurescriptError $ do
        let
          f = dir </> "a.purs"
        liftIO $ T.writeFile f "module A where"
        void $ compile (PurescriptUnpackDir tmp) [f] (CodeGenDir dir)
        pure (QC.property True)

prop_purescript_warnings =
  QC.once . neg . testIO . withTempDirectory "dist" "purescript" $ \dir ->
    withTempDirectory "dist" "purescript" $ \tmp ->
      testEitherT renderPurescriptError $ do
        let
          f = dir </> "a.purs"
        liftIO $ T.writeFile f "module A where\n\nb = \"\""
        void $ compile (PurescriptUnpackDir tmp) [f] (CodeGenDir dir)
        pure (QC.property True)

prop_purescript_failure =
  QC.once . neg . testIO . withTempDirectory "dist" "purescript" $ \dir ->
    withTempDirectory "dist" "purescript" $ \tmp ->
      testEitherT renderPurescriptError $ do
        let
          f = dir </> "a.purs"
        liftIO $ T.writeFile f "bad"
        void $ compile (PurescriptUnpackDir tmp) [f] (CodeGenDir dir)
        pure (QC.property True)


return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
