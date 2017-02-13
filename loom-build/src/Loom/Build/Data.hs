{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Data (
    FilePattern (..)
  , Loom (..)
  , LoomResolved (..)
  , LoomName (..)
  , LoomConfig (..)
  , LoomConfigResolved (..)
  , compileFilePattern
  , renderFilePattern
  , appendFilePattern
  , findFiles
  ) where

import qualified Data.Text as T

import           P

import           System.FilePath (FilePath, makeRelative)
import qualified System.FilePath.Glob as G
import qualified System.FilePath.Glob.Primitive as G
import           System.IO (IO)

newtype FilePattern =
  FilePattern G.Pattern
    deriving (Eq, Show)

data Loom =
  Loom {
      loomOutput :: FilePath
    , loomConfig :: LoomConfig
    , loomConfigs :: [LoomConfig]
    } deriving (Eq, Show)

data LoomResolved =
  LoomResolved {
      loomResolvedOutput :: FilePath
    , loomResolvedConfig :: LoomConfigResolved
    , loomResolvedConfigs :: [LoomConfigResolved]
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
      loomConfigResolvedRoot :: FilePath
    , loomConfigResolvedName :: LoomName
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

appendFilePattern :: FilePattern -> FilePattern -> FilePattern
appendFilePattern (FilePattern f1) (FilePattern f2) =
  FilePattern $
    f1 <> G.literal "/" <> f2

findFiles :: FilePath -> [FilePattern] -> IO [[FilePath]]
findFiles root fps =
  fmap (fmap (makeRelative root)) . fst <$>
     G.globDir (fmap (\(FilePattern g) -> g) fps) root
