{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Loom.Config.Toml (
    LoomConfigTomlError (..)
  , resolveConfig
  , renderLoomConfigTomlError
  ) where

import           Control.Lens (Traversal', (^?), preview)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Core.Data

import           P

import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.FilePath ((</>), FilePath, makeRelative, takeDirectory)
import           System.IO (IO)

import           Text.Parsec.Error (ParseError)
import           Text.Toml (parseTomlDoc)
import           Text.Toml.Types (Table, TValue (..))

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT)
import           X.Text.Toml (_NTable, _NTValue, _VArray, _VString, _VInteger, key)

{--
# example "loom.toml" config file

[loom]
  version = 2
  dependencies = ["lib/bikeshed"]
  name = "my_project"

[components]
  paths = ["components/*"]

[sass]
  paths = ["scss/*"]

[js]
  paths = ["js/*"]

[js.bundle.first]
  paths = ["js/first/*"]
  main = "js/first.js"

[js.dependencies]
  npm = [
      ["react", "v1.0.0", "badfeed"]
    ]

[purs]
  paths = ["purs"]

[purs.dependencies]
  github2 = [
      ["purescript/purescript-prelude", "tags/v1.0.0", "deadbeef"]
    ]

[purs.test]
  paths = ["test"]

[purs.test.dependencies]
  github2 = [
      ["purescript/purescript-quickcheck", "tags/v1.0.0", "63277c8df15822bf5c9e9f673e08ef8080baf0ba"]
    ]
--}

data LoomConfigTomlError =
    ConfigTomlParseError ParseError
  | ConfigMissingVersionError
  | ConfigUnknownVersionError Int64
  | ConfigInvalidField Text
  | ConfigInvalidPattern Text
  | ConfigFileNotFound FilePath
  | BundleNoMain Text
    deriving (Eq, Show)

data LoomConfigRaw =
  LoomConfigRaw {
      loomConfigRawName :: LoomName
    , loomConfigRawDependencies :: [FilePath]
    , loomConfigRawComponents :: [FilePattern]
    , loomConfigRawSass :: [FilePattern]
    , loomConfigRawJsPaths :: [FilePattern]
    , loomConfigRawJsBundles :: [Bundle]
    , loomConfigRawJsNpm :: [NpmDependency]
    , loomConfigRawJsGithub :: [GithubDependency]
    , loomConfigRawPurs :: PurescriptBundle FilePattern
    , loomConfigRawPursTest :: PurescriptBundle FilePattern
    } deriving (Eq, Show)

defaultLoomFile :: FilePath
defaultLoomFile =
  "loom.toml"

resolveConfig :: FilePath -> EitherT LoomConfigTomlError IO Loom
resolveConfig root = do
  let
    read' path' =
      newEitherT $ ifM
        (doesFileExist path')
        (pure <$> T.readFile path')
        (pure . Left . ConfigFileNotFound $ path')
    parse' path = do
      (dir, t) <- ifM
        (liftIO . doesDirectoryExist $ path)
        (fmap ((,) path) . read' $ path </> defaultLoomFile)
        (fmap ((,) (takeDirectory path)) . read' $ path)
      c <- hoistEither . parseConfig $ t
      ds <- mapM (parse' . (</>) dir) . loomConfigRawDependencies $ c
      pure $ ((dir, c), bind (uncurry (:)) ds)
  (rc1@(dir1, _), rcs) <- parse' root
  let
    config' (dir, rc) =
      LoomConfig
        (LoomRoot $ makeRelative dir1 dir)
        (loomConfigRawName rc)
        (loomConfigRawComponents rc)
        (loomConfigRawSass rc)
        (loomConfigRawJsPaths rc)
        (loomConfigRawJsBundles rc)
        (loomConfigRawJsNpm rc)
        (loomConfigRawJsGithub rc)
        (loomConfigRawPurs rc)
        (loomConfigRawPursTest rc)
  pure . Loom (config' rc1) . fmap config' $ rcs

parseConfig :: Text -> Either LoomConfigTomlError LoomConfigRaw
parseConfig t =
  first ConfigTomlParseError (parseTomlDoc "loom.toml" t) >>= parseTomlConfig

parseTomlConfig :: Table -> Either LoomConfigTomlError LoomConfigRaw
parseTomlConfig t =
  case t ^? key "loom" . _NTable . key "version" . _NTValue . _VInteger of
    Nothing ->
      Left ConfigMissingVersionError
    Just 1 ->
      parseTomlConfigV1 t
    Just 2 ->
      parseTomlConfigV2 t
    Just n ->
      Left $ ConfigUnknownVersionError n

parseTomlConfigV1 :: Table -> Either LoomConfigTomlError LoomConfigRaw
parseTomlConfigV1 t =
  LoomConfigRaw
    <$> (fmap LoomName . maybeToRight (ConfigInvalidField "loom.name") $
      t ^? key "loom" . _NTable . key "name" . _NTValue . _VString
      )
    <*> (maybe (pure []) (fmap (fmap T.unpack) . maybeToRight (ConfigInvalidField "loom.dependencies") . mapM (preview _VString)) $
      t ^? key "loom" . _NTable . key "dependencies" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseFilePatterns "component.paths") $
      t ^? key "component" . _NTable . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseFilePatterns "sass.paths") $
      t ^? key "sass" . _NTable . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseFilePatterns "js.paths") $
      t ^? key "js" . _NTable . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) parseBundleTable $
      t ^? key "js" . _NTable . key "bundle" . _NTable
      )
    <*> (maybe (pure []) (parseNpmDeps "js.dependencies.npm") $
      t ^? key "js" . _NTable . key "dependencies" . _NTable . key "npm" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseGithubDeps "js.dependencies.github" GithubDependencyV1) $
      t ^? key "js" . _NTable . key "dependencies" . _NTable . key "github" . _NTValue . _VArray
      )
    <*> parseTomlPurescriptBundleV1 (key "purs" . _NTable) "purs" t
    <*> parseTomlPurescriptBundleV1 (key "purs" . _NTable . key "test" . _NTable) "purs.test" t

parseTomlConfigV2 :: Table -> Either LoomConfigTomlError LoomConfigRaw
parseTomlConfigV2 t =
  LoomConfigRaw
    <$> (fmap LoomName . maybeToRight (ConfigInvalidField "loom.name") $
      t ^? key "loom" . _NTable . key "name" . _NTValue . _VString
      )
    <*> (maybe (pure []) (fmap (fmap T.unpack) . maybeToRight (ConfigInvalidField "loom.dependencies") . mapM (preview _VString)) $
      t ^? key "loom" . _NTable . key "dependencies" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseFilePatterns "component.paths") $
      t ^? key "component" . _NTable . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseFilePatterns "sass.paths") $
      t ^? key "sass" . _NTable . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseFilePatterns "js.paths") $
      t ^? key "js" . _NTable . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) parseBundleTable $
      t ^? key "js" . _NTable . key "bundle" . _NTable
      )
    <*> (maybe (pure []) (parseNpmDeps "js.dependencies.npm") $
      t ^? key "js" . _NTable . key "dependencies" . _NTable . key "npm" . _NTValue . _VArray
      )
    <*> (mappend
      <$> (maybe (pure []) (parseGithubDeps "js.dependencies.github" GithubDependencyV1) $
        t ^? key "js" . _NTable . key "dependencies" . _NTable . key "github" . _NTValue . _VArray
        )
      <*> (maybe (pure []) (parseGithubDeps "js.dependencies.github2" GithubDependencyV2) $
        t ^? key "js" . _NTable . key "dependencies" . _NTable . key "github" . _NTValue . _VArray
        )
      )
    <*> parseTomlPurescriptBundleV2 (key "purs" . _NTable) "purs" t
    <*> parseTomlPurescriptBundleV2 (key "purs" . _NTable . key "test" . _NTable) "purs.test" t

parseTomlPurescriptBundleV1 ::
  Traversal' Table Table -> Text -> Table -> Either LoomConfigTomlError (PurescriptBundle FilePattern)
parseTomlPurescriptBundleV1 root n t =
  PurescriptBundle
    <$> (maybe (pure []) (parseFilePatterns (n <> ".paths")) $
      t ^? root . key "paths" . _NTValue . _VArray
      )
    <*> (maybe (pure []) (parseGithubDeps (n <> ".dependencies.github") GithubDependencyV1) $
      t ^? root . key "dependencies" . _NTable . key "github" . _NTValue . _VArray
      )
    <*> (pure $
      t ^? root . key "main" . _NTValue . _VString
      )

parseTomlPurescriptBundleV2 ::
  Traversal' Table Table -> Text -> Table -> Either LoomConfigTomlError (PurescriptBundle FilePattern)
parseTomlPurescriptBundleV2 root n t =
  PurescriptBundle
    <$> (maybe (pure []) (parseFilePatterns (n <> ".paths")) $
      t ^? root . key "paths" . _NTValue . _VArray
      )
    <*> (mappend
      <$> (maybe (pure []) (parseGithubDeps (n <> ".dependencies.github") GithubDependencyV1) $
        t ^? root . key "dependencies" . _NTable . key "github" . _NTValue . _VArray
        )
      <*> (maybe (pure []) (parseGithubDeps (n <> ".dependencies.github2") GithubDependencyV2) $
        t ^? root . key "dependencies" . _NTable . key "github2" . _NTValue . _VArray
        )
      )
    <*> (pure $
      t ^? root . key "main" . _NTValue . _VString
      )

parseFilePatterns :: Text -> [TValue] -> Either LoomConfigTomlError [FilePattern]
parseFilePatterns l =
  mapM (bind parseFilePattern . maybeToRight (ConfigInvalidField l) . preview _VString)

parseFilePattern :: Text -> Either LoomConfigTomlError FilePattern
parseFilePattern t =
  first ConfigInvalidPattern . compileFilePattern $ t

parseNpmDeps :: Text -> [TValue] -> Either LoomConfigTomlError [NpmDependency]
parseNpmDeps l vals =
  for vals $ \case
    VArray (VString name : VString vers : VString sha : []) ->
      pure (NpmDependency (NpmPackage name) (NpmPackageVersion vers) (Sha1 sha))
    _ ->
      Left (ConfigInvalidField l)

parseGithubDeps :: Text -> GithubDependencyType -> [TValue] -> Either LoomConfigTomlError [GithubDependency]
parseGithubDeps l typ vals =
  for vals $ \case
    VArray (VString name : VString ref : VString sha : []) ->
      case T.splitOn "/" name of
        [user, repo] ->
          pure (GithubDependency (GithubRepo user repo) (GitRef ref) (Sha1 sha) typ)
        _ ->
          Left (ConfigInvalidField l)
    _ ->
      Left (ConfigInvalidField l)

parseBundleTable :: Table -> Either LoomConfigTomlError [Bundle]
parseBundleTable t =
  for (HM.keys t) $ \b ->
    Bundle (BundleName b)
      <$> (maybe (Left (BundleNoMain b)) parseFilePattern
            (t ^? key b . _NTable . key "main" . _NTValue . _VString))
      <*> (maybe (pure []) (parseFilePatterns (b <> ".paths")) $
            (t ^? key b . _NTable . key "paths" . _NTValue . _VArray))

renderLoomConfigTomlError :: LoomConfigTomlError -> Text
renderLoomConfigTomlError te =
  case te of
    ConfigTomlParseError pe ->
      mconcat ["Loom configuration could not be parsed, toml parse error: ", T.pack . show $ pe]
    ConfigMissingVersionError ->
      "Loom configuration does not contain a version field"
    ConfigUnknownVersionError n ->
      mconcat ["Loom configuration contains an unkown version: ", renderIntegral n]
    ConfigInvalidField f ->
      mconcat ["Loom configuration contains an invalid field: ", f]
    ConfigInvalidPattern e ->
      mconcat ["Loom configuration contains an invalid pattern: ", e]
    ConfigFileNotFound f ->
      mconcat ["Loom configuration file not found: ", T.pack f]
    BundleNoMain b ->
      mconcat ["Bundle '", b, "' has no main entry point."]
