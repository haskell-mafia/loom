{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Data (
    FilePattern (..)
  , Loom (..)
  , LoomName (..)
  , LoomConfig (..)
  , LoomConfigResolved (..)
  , compileFilePattern
  , renderFilePattern
  , appendFilePattern
  ) where

import qualified Data.Text as T

import           P

import           System.FilePath (FilePath)
import qualified System.FilePath.Glob as G

newtype FilePattern =
  FilePattern G.Pattern
    deriving (Eq, Show)

data Loom =
  Loom {
      loomOutput :: FilePath
    , loomConfigs :: [LoomConfig]
    } deriving (Eq, Show)

newtype LoomName =
  LoomName {
      renderLoomName :: Text
    } deriving (Eq, Show)

data LoomConfig =
  LoomConfig {
      loomConfigRoot :: FilePath
    , loomConfigName :: LoomName
    , loomConfigComponents :: [FilePattern]
    , loomConfigSass :: [FilePattern]
    } deriving (Eq, Show)

data LoomConfigResolved =
  LoomConfigResolved {
      loomConfigResolvedName :: LoomName
    , loomConfigResolvedComponents :: [FilePath]
    , loomConfigResolvedSass :: [FilePath]
    } deriving (Eq, Show)

compileFilePattern :: Text -> Either Text FilePattern
compileFilePattern =
  let
    options =
      G.CompOptions {
          G.characterClasses = False
        , G.characterRanges = False
        , G.numberRanges = False
        , G.wildcards = True
        , G.recursiveWildcards = False
        , G.pathSepInRanges = False
        , G.errorRecovery = False
        }
  in
    bimap T.pack FilePattern . G.tryCompileWith options . T.unpack

renderFilePattern :: FilePattern -> Text
renderFilePattern (FilePattern t) =
  T.pack . G.decompile $ t

appendFilePattern :: FilePattern -> FilePattern -> Either Text FilePattern
appendFilePattern f1 f2 =
  compileFilePattern $
    renderFilePattern f1 <> "/" <> renderFilePattern f2
