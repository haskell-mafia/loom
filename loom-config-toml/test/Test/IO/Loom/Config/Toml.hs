{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Config.Toml where

import           Loom.Build.Data
import           Loom.Config.Toml

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)

import           P
import qualified Prelude as Unsafe (error)

import           System.IO (IO)

import           Test.QuickCheck ((===))
import qualified Test.QuickCheck as QC

import           X.Control.Monad.Trans.Either (runEitherT)

prop_loom_config_toml_parse_ok =
  QC.once . QC.conjoin $ [
      check "test/data/config/v1/empty.toml" . Right $ Loom "dist"
        (LoomConfig
          "."
          (LoomName "empty")
          (AssetsPrefix "assets")
          []
          []
          )
        []
    , check "test/data/config/v1/basic.toml" . Right $ Loom "dist"
        (LoomConfig
          "."
          (LoomName "basic")
          (AssetsPrefix "assets")
          [p "components/*"]
          [p "scss/*"]
          )
        []
    , check "test/data/config/v1/dependencies.toml" . Right $ Loom "dist"
        (LoomConfig
          "."
          (LoomName "dependencies_1")
          (AssetsPrefix "assets")
          [p "components/*"]
          [p "scss/*"]
          )
      [
        LoomConfig
          "subdir"
          (LoomName "dependencies_2")
          (AssetsPrefix "assets")
          [p "components/*"]
          [p "scss/*"]
        ]
    ]

prop_loom_config_toml_parse_error =
  QC.once . QC.conjoin $ [
      check "test/data/config/v1/invalid.no-version.toml" . Left $
        ConfigMissingVersionError
    , check "test/data/config/v1/invalid.unknown-version.toml" . Left $
        ConfigUnknownVersionError 2
    , check "test/data/config/v1/invalid.missing-name.toml" . Left $
        ConfigInvalidField "loom.name"
    , check "test/data/config/v1/invalid.paths-field.toml" . Left $
        ConfigInvalidField "component.paths"
    , check "test/data/config/v1/invalid.dependencies.toml" . Left $
        ConfigFileNotFound "test/data/config/v1/missing.toml"
    ]

check path expected =
  testIO $ do
    c <- runEitherT . resolveConfig $ path
    pure . QC.counterexample path $ c === expected

-- FIX QQ
p :: Text -> FilePattern
p =
  either (Unsafe.error "invalid pattern") id . compileFilePattern

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
