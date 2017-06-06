{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Config.Toml where

import           Loom.Core.Data
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
      check "test/data/config/v1/empty.toml" . Right $ Loom
        (LoomConfig
          (LoomRoot ".")
          (LoomName "empty")
          []
          []
          []
          []
          []
          []
          (PurescriptBundle [] [] Nothing)
          (PurescriptBundle [] [] Nothing)
          )
        []
    , check "test/data/config/v1/basic.toml" . Right $ Loom
        (LoomConfig
          (LoomRoot ".")
          (LoomName "basic")
          [p "components/*"]
          [p "scss/*"]
          []
          []
          []
          []
          (PurescriptBundle [] [] Nothing)
          (PurescriptBundle [] [] Nothing)
          )
        []
    , check "test/data/config/v1/dependencies.toml" . Right $ Loom
        (LoomConfig
          (LoomRoot ".")
          (LoomName "dependencies_1")
          [p "components/*"]
          [p "scss/*"]
          [p "app/src/js/*"]
          [Bundle (BundleName "first") (p "app/src/first.js") [p "app/src/first/js/*"]]
          [NpmDependency (NpmPackage "d3") (NpmPackageVersion "4.7.4") (Sha1 "a2f40eb57decc51bc469010d48ae74a20e025772")]
          [GithubDependency (GithubRepo "isaacs" "rimraf") (GitRef "tags/v2.6.1") (Sha1 "813139ac3628ae0b47136de18939cbb623e21475") GithubDependencyV1]
          (PurescriptBundle
            [p "app/src/purs/*"]
            [GithubDependency (GithubRepo "purescript" "purescript-newtype") (GitRef "tags/v2.0.0") (Sha1 "2276bd44ff5b7440c455839833c69f40cc8d8616") GithubDependencyV1]
            Nothing
            )
          (PurescriptBundle [] [] Nothing)
          )
      [
        LoomConfig
          (LoomRoot "subdir")
          (LoomName "dependencies_2")
          [p "components/*"]
          [p "scss/*"]
          []
          []
          []
          []
          (PurescriptBundle [] [] Nothing)
          (PurescriptBundle [] [] Nothing)
        ]
    , check "test/data/config/v1/test.toml" . Right $ Loom
        (LoomConfig
          (LoomRoot ".")
          (LoomName "test")
          []
          []
          []
          []
          []
          []
          (PurescriptBundle [] [] Nothing)
          (PurescriptBundle
            [p "app/test/purs/*"]
            [GithubDependency (GithubRepo "purescript" "purescript-newtype") (GitRef "tags/v2.0.0") (Sha1 "2276bd44ff5b7440c455839833c69f40cc8d8616") GithubDependencyV1]
            (Just "Test.App")
            )
          )
        []
    , check "test/data/config/v2/github.toml" . Right $ Loom
        (LoomConfig
          (LoomRoot ".")
          (LoomName "github")
          []
          []
          []
          [Bundle (BundleName "first") (p "app/src/first.js") [p "app/src/first/js/*"]]
          []
          [GithubDependency (GithubRepo "isaacs" "rimraf") (GitRef "tags/v2.6.1") (Sha1 "813139ac3628ae0b47136de18939cbb623e21475") GithubDependencyV2]
          (PurescriptBundle
            [p "app/src/purs/*"]
            [GithubDependency (GithubRepo "purescript" "purescript-newtype") (GitRef "tags/v2.0.0") (Sha1 "2276bd44ff5b7440c455839833c69f40cc8d8616") GithubDependencyV2]
            Nothing
            )
          (PurescriptBundle [] [] Nothing)
          )
      []
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
