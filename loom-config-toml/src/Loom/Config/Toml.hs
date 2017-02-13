{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Config.Toml (
    LoomConfigTomlError (..)
  , resolveConfig
  , renderLoomConfigTomlError
  ) where

import           Control.Lens ((^?), preview)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Build.Data

import           P

import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.FilePath ((</>), FilePath, makeRelative, takeDirectory)
import           System.IO (IO)

import           Text.Parsec.Error (ParseError)
import           Text.Toml (parseTomlDoc)
import           Text.Toml.Types (Table, TValue)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT)
import           X.Text.Toml (_NTable, _NTValue, _VArray, _VString, _VInteger, key)

{--
# example "loom.toml" config file

[loom]
  version = 1
  dependencies = ["lib/bikeshed"]
  name = "my_project"
  output = "dist"

[components]
  paths = ["components/*"]

[sass]
  paths = ["scss/*"]
--}

data LoomConfigTomlError =
    ConfigTomlParseError ParseError
  | ConfigMissingVersionError
  | ConfigUnknownVersionError Int64
  | ConfigInvalidField Text
  | ConfigInvalidPattern Text
  | ConfigFileNotFound FilePath
    deriving (Eq, Show)

data LoomConfigRaw =
  LoomConfigRaw {
      loomConfigRawName :: LoomName
    , loomConfigRawOutput :: FilePath
    , loomConfigRawDependencies :: [FilePath]
    , loomConfigRawComponents :: [FilePattern]
    , loomConfigRawSass :: [FilePattern]
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
  (rc1@(dir1, c), rcs) <- parse' root
  let
    config' (dir, rc) =
      LoomConfig
        (makeRelative dir1 dir)
        (loomConfigRawName rc)
        (loomConfigRawComponents rc)
        (loomConfigRawSass rc)
  pure . Loom (loomConfigRawOutput c) (config' rc1) . fmap config' $ rcs

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
    Just n ->
      Left $ ConfigUnknownVersionError n

parseTomlConfigV1 :: Table -> Either LoomConfigTomlError LoomConfigRaw
parseTomlConfigV1 t =
  LoomConfigRaw
    <$> (fmap LoomName . maybeToRight (ConfigInvalidField "loom.name") $
      t ^? key "loom" . _NTable . key "name" . _NTValue . _VString
      )
    <*> (fmap T.unpack . maybeToRight (ConfigInvalidField "loom.output") $
      t ^? key "loom" . _NTable . key "output" . _NTValue . _VString
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

parseFilePatterns :: Text -> [TValue] -> Either LoomConfigTomlError [FilePattern]
parseFilePatterns l =
  mapM (bind parseFilePattern . maybeToRight (ConfigInvalidField l) . preview _VString)

parseFilePattern :: Text -> Either LoomConfigTomlError FilePattern
parseFilePattern t =
  first ConfigInvalidPattern . compileFilePattern $ t

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
