{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Machinator where

import           Control.Monad.Trans.Class (lift)

import           Loom.Machinator

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath ((</>))
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           X.Control.Monad.Trans.Either (runEitherT)

prop_machinator_success =
  QC.forAll genModuleName $ \name ->
  testIO . withMachinator $ \dir -> do
    let
       dir1 = dir </> "dir1"
       dir2 = dir </> "dir2"
       d1 = dir1 <> "/test1.mcn"
       d2 = dir2 <> "/test2.mcn"
    lift $ createDirectoryIfMissing True dir1
    lift $ createDirectoryIfMissing True dir2
    lift $ T.writeFile d1 "-- machinator @ v1\ndata Foo = Foo"
    lift $ T.writeFile d2 "-- machinator @ v1\ndata Bar = Bar"
    out <- compileMachinator [
        MachinatorInput name dir1 [d1]
      , MachinatorInput name dir2 [d2]
      ]
    f3 <- generateMachinatorHaskell dir out
    lift . fmap QC.conjoin . mapM (doesFileExist . (</>) dir) $ f3

prop_machinator_missing =
  QC.forAll genModuleName $ \name ->
  QC.once . testIO . withMachinator $ \dir -> do
    m <- lift . runEitherT $ compileMachinator [MachinatorInput name dir ["missing.scss"]]
    pure $ isLeft m

prop_machinator_fail =
  QC.forAll genModuleName $ \name ->
  QC.once . testIO . withMachinator $ \dir -> do
    let
       f1 = dir <> "/test1.prj"
    lift $ T.writeFile f1 "\\x"
    m <- lift . runEitherT $ compileMachinator [MachinatorInput name dir [f1]]
    pure $ isLeft m

-------------

genModuleName :: Gen ModuleName
genModuleName =
  fmap (ModuleName . T.pack) . QC.listOf1 . QC.choose $ ('a', 'z')

withMachinator f =
  withTempDirectory "dist" "loom-machinator" $ \dir ->
    testEitherT renderMachinatorError $
      f dir

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
