{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Build.Component where

import           Control.Monad.IO.Class (liftIO)

import           Data.List (nub, sort)
import qualified Data.Text.IO as T

import           Loom.Build.Component

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import           System.Directory (doesFileExist)
import           System.FilePath (FilePath, (</>))
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck.Jack ((===))
import qualified Test.QuickCheck.Jack as J

import           X.Control.Monad.Trans.Either (runEitherT)

prop_build_component =
  J.forAll (J.listOf genFileNameNoExt) $ \fs ->
  testIO $
    withTempDirectory "dist" "loom-build" $ \dir ->
    testEitherT renderComponentError $ do
      liftIO . for_ fs $ \f ->
        T.writeFile (dir </> f <> ".scss") ""
      liftIO . for_ fs $ \f ->
        T.writeFile (dir </> f <> ".prj") ""
      c <- resolveComponent dir
      fmap (J.conjoin . join) . liftIO . sequence $ [
          for (componentSassFiles c) $
            doesFileExist . componentFilePath c
        , for (componentProjectorFiles c) $
            doesFileExist . componentFilePath c
        ]

prop_build_component_missing =
  J.once . testIO $ do
    let
      f = "does_not_exist"
    c <- runEitherT . resolveComponent $ f
    pure $ c === Left (ComponentMissing f)

prop_build_component_unknown =
  J.forAll (J.listOf genFileNameNoExt) $ \fs ->
  J.forAll (nub <$> J.listOf1 genFileNameNoExt) $ \gs ->
  testIO $
    withTempDirectory "dist" "loom-build" $ \dir -> do
      for_ fs $ \f ->
        T.writeFile (dir </> f <> ".scss") ""
      for_ gs $ \f ->
        T.writeFile (dir </> f) ""
      c <- runEitherT . resolveComponent $ dir
      pure $ case c of
        Left (ComponentUnknownFiles gs') ->
          sort gs' === sort gs
        _ ->
          J.counterexample (show c) False

genFileNameNoExt :: J.Gen FilePath
genFileNameNoExt =
  J.listOf1 . J.chooseChar $ ('a', 'z')

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
