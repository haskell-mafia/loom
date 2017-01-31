{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Build.Data where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Build.Data

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), takeDirectory)
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck.Jack ((===))

prop_build_find =
  testIO $
    withTempDirectory "dist" "loom-build" $ \dir -> do
      -- FIX Proper gen!!!
      p <- either (fail . T.unpack) pure $
        compileFilePattern "foo/bar/*"
      let
        fsx = ["foo/bar/a.txt", "foo/bar/b.scss"]
        fsy = ["foo/bar.txt"]
      for_ (fsx <> fsy) $ \f -> do
        createDirectoryIfMissing True . takeDirectory $ dir </> f
        T.writeFile (dir </> f) ""
      fsz <- findFiles dir [p]
      pure $ fsz === [fsx]

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
