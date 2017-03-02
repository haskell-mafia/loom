{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Data (
    FilePattern (..)
  , Loom (..)
  , LoomResolved (..)
  , LoomTmp (..)
  , LoomRoot (..)
  , LoomFile (..)
  , LoomName (..)
  , LoomConfig (..)
  , LoomConfigResolved (..)
  , LoomResult (..)
  , Component (..)
  , ComponentFile (..)
  , ImageFile (..)
  , LoomSitePrefix (..)
  , AssetsPrefix (..)
  , loomFilePath
  , componentName
  , componentFilePath
  , componentFilePathNoRoot
  , imageFilePath
  , imageAssetPath
  , imageAssetFilePath
  , cssAssetPath
  , cssAssetFilePath
  , compileFilePattern
  , renderFilePattern
  , appendFilePattern
  , loomWatchPatterns
  , matchFilePatterns
  , findFiles
  , findFiles'
  ) where

import qualified Data.Text as T

import           Loom.Machinator (MachinatorOutput)
import           Loom.Projector (ProjectorOutput)
import           Loom.Sass (CssFile (..))

import           P

import           System.FilePath (FilePath, (</>), makeRelative, normalise)
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

-- | Directory location for storing any temporary files during the loom build
--
-- Defaults to '.loom/'
--
newtype LoomTmp =
  LoomTmp {
      loomTmpFilePath :: FilePath
    } deriving (Eq, Show)

-- | Represents the path to a loom file from the CWD
-- eg.
-- '.'
-- 'lib/bikeshed'
newtype LoomRoot =
  LoomRoot {
      loomRootFilePath :: FilePath
    } deriving (Eq, Show)

-- | Represents a resolved single file within a loom project
data LoomFile =
  LoomFile {
      loomFileRoot :: LoomRoot
    , loomFileRawPath :: FilePath
    } deriving (Eq, Show)

newtype LoomName =
  LoomName {
      renderLoomName :: Text
    } deriving (Eq, Show)

data LoomConfig =
  LoomConfig {
      loomConfigRoot :: LoomRoot
    , loomConfigName :: LoomName
    , loomConfigAssetsPrefix :: AssetsPrefix
    , loomConfigComponents :: [FilePattern]
    , loomConfigSass :: [FilePattern]
    } deriving (Eq, Show)

data LoomConfigResolved =
  LoomConfigResolved {
      loomConfigResolvedRoot :: LoomRoot
    , loomConfigResolvedName :: LoomName
    , loomConfigResolvedAssetsPrefix :: AssetsPrefix
    , loomConfigResolvedComponents :: [LoomFile]
    , loomConfigResolvedSass :: [LoomFile]
    } deriving (Eq, Show)

data LoomResult =
  LoomResult {
      loomResultName :: LoomName
    , loomResultComponents :: [Component]
    , loomResultMachinatorOutput :: MachinatorOutput
    , loomResultProjectorOutput :: ProjectorOutput
    , loomResultCss :: CssFile
    , loomResultImages :: [ImageFile]
    }

data Component =
  Component {
      componentPath :: LoomFile
    , componentSassFiles :: [ComponentFile]
    , componentProjectorFiles :: [ComponentFile]
    , componentMachinatorFiles :: [ComponentFile]
    , componentImageFiles :: [ComponentFile]
    } deriving (Eq, Show)

data ComponentFile =
  ComponentFile {
      componentLoomFile :: LoomFile
    , componentRawFilePath :: FilePath
    } deriving (Eq, Show)

-- | Represents the logical prefix to be used for the root for all absolute paths
-- By default this will be "/" but for generating paths for commit-specific sites,
-- such as on a build, this can be configured.
newtype LoomSitePrefix =
  LoomSitePrefix {
      loomSitePrefix :: Text
    } deriving (Eq, Show)

newtype AssetsPrefix =
  AssetsPrefix {
      assetsPrefix :: FilePath
    } deriving (Eq, Show)

loomFilePath :: LoomFile -> FilePath
loomFilePath (LoomFile r f) =
  normalise $ loomRootFilePath r </> f

componentName :: Component -> Text
componentName =
  T.pack . loomFileRawPath . componentPath

componentFilePath :: ComponentFile -> FilePath
componentFilePath (ComponentFile r f) =
  loomFilePath r </> f

componentFilePathNoRoot :: ComponentFile -> FilePath
componentFilePathNoRoot (ComponentFile r f) =
  loomFileRawPath r </> f

data ImageFile =
  ImageFile {
      imageLoomName :: LoomName
    , imageComponentFile :: ComponentFile
    } deriving (Eq, Show)

imageFilePath :: ImageFile -> FilePath
imageFilePath (ImageFile _ f) =
  componentFilePath f

cssAssetPath :: LoomSitePrefix -> AssetsPrefix -> CssFile -> Text
cssAssetPath p apx f =
  loomSitePrefix p <> (T.pack . cssAssetFilePath apx) f

cssAssetFilePath :: AssetsPrefix -> CssFile -> FilePath
cssAssetFilePath apx f =
  assetsPrefix apx </> renderCssFile f

imageAssetPath :: LoomSitePrefix -> AssetsPrefix -> ImageFile -> Text
imageAssetPath p apx f =
  loomSitePrefix p <> (T.pack . imageAssetFilePath apx) f

imageAssetFilePath :: AssetsPrefix -> ImageFile -> FilePath
imageAssetFilePath apx (ImageFile n f) =
  assetsPrefix apx </> (T.unpack . renderLoomName) n </> componentFilePathNoRoot f

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

loomWatchPatterns :: Loom -> [FilePattern]
loomWatchPatterns (Loom _ c cs) =
  c : cs >>= \(LoomConfig r _ _ comps sass) ->
    let
      rfp = FilePattern . G.literal . loomRootFilePath $ r
    in
      mconcat [
          comps >>= \cp ->
            fmap (appendFilePattern rfp . appendFilePattern cp) componentFilePatterns
        , sass
        ]

componentFilePatterns :: [FilePattern]
componentFilePatterns =
  [
      FilePattern $ G.wildcard <> G.literal ".prj"
    , FilePattern $ G.wildcard <> G.literal ".mcn"
    , FilePattern $ G.wildcard <> G.literal ".scss"
    , FilePattern $ G.wildcard <> G.literal ".svg"
    , FilePattern $ G.wildcard <> G.literal ".png"
    , FilePattern $ G.wildcard <> G.literal ".jpg"
    , FilePattern $ G.literal "README.md"
    , FilePattern $ G.literal "example/" <> G.wildcard <> G.literal ".prj"
    , FilePattern $ G.literal "mock/" <> G.wildcard <> G.literal ".prj"
    ]

matchFilePatterns :: [FilePattern] -> FilePath -> Bool
matchFilePatterns fps f =
  any (\(FilePattern g) -> G.match g f) fps

findFiles :: LoomRoot -> [FilePattern] -> IO [[LoomFile]]
findFiles root =
  fmap (fmap (fmap ((LoomFile root)))) . findFiles' (loomRootFilePath root)

findFiles' :: FilePath -> [FilePattern] -> IO [[FilePath]]
findFiles' root fps =
  fmap (fmap (makeRelative root)) . fst <$>
     G.globDir (fmap (\(FilePattern g) -> g) fps) root
