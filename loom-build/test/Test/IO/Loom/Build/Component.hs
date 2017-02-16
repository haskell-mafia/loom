{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Build.Component where

import           Control.Monad.IO.Class (liftIO)

import           Data.List (nub)
import qualified Data.Text.IO as T

import           Loom.Build.Component
import           Loom.Build.Data

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
      liftIO . for_ fs $ \f ->
        T.writeFile (dir </> f <> ".mcn") ""
      liftIO . for_ fs $ \f ->
        T.writeFile (dir </> f <> ".svg") ""
      liftIO . for_ fs $ \f ->
        T.writeFile (dir </> f <> ".png") ""
      c <- resolveComponent (LoomFile (LoomRoot ".") dir)
      fmap (J.conjoin . join) . liftIO . sequence $ [
          for (componentSassFiles c) $
            doesFileExist . componentFilePath
        , for (componentProjectorFiles c) $
            doesFileExist . componentFilePath
        , for (componentMachinatorFiles c) $
            doesFileExist . componentFilePath
        , for (componentImageFiles c) $
            doesFileExist . componentFilePath
        ]

prop_build_component_missing =
  J.once . testIO $ do
    let
      f = LoomFile (LoomRoot ".") "does_not_exist"
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
      c <- runEitherT . resolveComponent . LoomFile (LoomRoot ".") $ dir
      pure $ isRight c

genFileNameNoExt :: J.Gen FilePath
genFileNameNoExt =
  J.listOf1 . J.chooseChar $ ('a', 'z')

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
