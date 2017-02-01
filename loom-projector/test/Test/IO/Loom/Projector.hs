{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Projector where

import           Control.Monad.Trans.Class (lift)

import           Loom.Projector

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           X.Control.Monad.Trans.Either (runEitherT)

prop_projector_success =
  QC.forAll genModuleName $ \name ->
  testIO . withProjector $ \dir -> do
    let
       f1 = dir <> "/test1.pjr"
    lift $ writeFile f1 "<a>b</a>"
    f3 <- compileProjector name [f1] dir
    lift . fmap QC.conjoin . mapM (doesFileExist . (</>) dir) $ f3

prop_projector_missing =
  QC.forAll genModuleName $ \name ->
  QC.once . testIO . withProjector $ \dir -> do
    m <- lift . runEitherT $ compileProjector name ["missing.scss"] dir
    pure $ isLeft m

prop_projector_fail =
  QC.forAll genModuleName $ \name ->
  QC.once . testIO . withProjector $ \dir -> do
    let
       f1 = dir <> "/test1.prj"
    lift $ writeFile f1 "\\x"
    m <- lift . runEitherT $ compileProjector name [f1] dir
    pure $ isLeft m

-------------

genModuleName :: Gen ModuleName
genModuleName =
  fmap (ModuleName . T.pack) . QC.listOf1 . QC.choose $ ('a', 'z')

withProjector f =
  withTempDirectory "dist" "loom-projector" $ \dir ->
    testEitherT renderProjectorError $
      f dir

writeFile =
  T.writeFile

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
