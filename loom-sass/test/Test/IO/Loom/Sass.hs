{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Sass where

import           Control.Monad.Trans.Class (lift)

import           Loom.Sass

import qualified Data.Text.IO as T

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.Directory (doesFileExist)
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck (Gen, once)
import qualified Test.QuickCheck as QC

import           X.Control.Monad.Trans.Either (eitherTFromMaybe, runEitherT)

prop_sass_success =
  QC.forAll genSassStyle $ \ss ->
  testIO . withSass $ \dir ps -> do
    let
       f1 = dir <> "/test1.scss"
       f2 = dir <> "/test2.scss"
       f3 = dir <> "/test3.css"
    lift $ writeFile f1 "$test: #ffffff;"
    lift $ writeFile f2 "@import \"test1.scss\";\n .foo { color: $test; }"
    firstT renderSassError $ compileSass ps ss f1 f3
    lift $ doesFileExist f3

prop_sass_missing =
  QC.forAll genSassStyle $ \ss ->
  once . testIO . withSass $ \dir ps -> do
    m <- lift . runEitherT $ compileSass ps ss "missing.scss" (dir <> "/test.css")
    pure $ isLeft m

prop_sass_fail =
  QC.forAll genSassStyle $ \ss ->
  once . testIO . withSass $ \dir ps -> do
    let
       f1 = dir <> "/test1.scss"
    lift $ writeFile f1 ".foo { color: $test; }"
    m <- lift . runEitherT $ compileSass ps ss f1 (dir <> "/test.css")
    pure $ isLeft m

-------------

genSassStyle :: Gen SassStyle
genSassStyle =
  QC.arbitraryBoundedEnum

-------------

withSass f =
  withTempDirectory "dist" "loom-sass" $ \dir ->
    testEitherT id $ do
      ps <- eitherTFromMaybe "Missing sass" $ findSassOnPath
      f dir ps

writeFile =
  T.writeFile

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
