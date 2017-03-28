{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Core.Data (
    FilePattern (..)
  , Loom (..)
  , LoomResolved (..)
  , LoomTmp (..)
  , LoomRoot (..)
  , LoomFile (..)
  , LoomName (..)
  , LoomConfig (..)
  , LoomConfigResolved (..)
  , Component (..)
  , ComponentFile (..)
  , ImageFile (..)
  , CssFile (..)
  , LoomSitePrefix (..)
  , AssetsPrefix (..)
  , loomFilePath
  , componentName
  , componentFilePath
  , componentFilePathNoRoot
  , templateName
  , imageFilePath
  , imageAssetPath
  , imageAssetFilePath
  , imageFilePathNoRoot
  , cssAssetPath
  , cssAssetFilePath
  , compileFilePattern
  , renderFilePattern
  , appendFilePattern
  , loomWatchPatterns
  , matchFilePatterns
  , componentFilePatterns
  , projectorFilePattern
  , machinatorFilePattern
  , sassFilePattern
  , imageFilePatterns
  , siteFilePatterns
  , findFiles
  , findFiles'
  ) where

import qualified Data.Text as T

import           P

import           System.FilePath (FilePath, (</>), makeRelative, normalise)
import qualified System.FilePath.Glob as G
import qualified System.FilePath.Glob.Primitive as G
import           System.IO (IO)

newtype FilePattern = FilePattern {
     unFilePattern :: G.Pattern
   } deriving (Eq, Show)

data Loom =
  Loom {
      loomConfig :: LoomConfig
    , loomConfigs :: [LoomConfig]
    } deriving (Eq, Show)

data LoomResolved =
  LoomResolved {
      loomResolvedConfig :: LoomConfigResolved
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
    } deriving (Eq, Ord, Show)

data LoomConfig =
  LoomConfig {
      loomConfigRoot :: LoomRoot
    , loomConfigName :: LoomName
    , loomConfigComponents :: [FilePattern]
    , loomConfigSass :: [FilePattern]
    } deriving (Eq, Show)

data LoomConfigResolved =
  LoomConfigResolved {
      loomConfigResolvedRoot :: LoomRoot
    , loomConfigResolvedName :: LoomName
    , loomConfigResolvedComponents :: [LoomFile]
    , loomConfigResolvedSass :: [LoomFile]
    } deriving (Eq, Show)

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

templateName :: LoomName -> Component -> Text
templateName ln cm =
  renderLoomName ln <> "/" <> componentName cm

newtype CssFile =
  CssFile {
      renderCssFile :: FilePath
    } deriving (Eq, Show)

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
imageAssetFilePath apx imf =
  assetsPrefix apx </> imageFilePathNoRoot imf

imageFilePathNoRoot :: ImageFile -> FilePath
imageFilePathNoRoot (ImageFile n f) =
  (T.unpack . renderLoomName) n </> componentFilePathNoRoot f


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
loomWatchPatterns (Loom c cs) =
  c : cs >>= \(LoomConfig r _ comps sass) ->
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
     projectorFilePattern
  :  machinatorFilePattern
  :  sassFilePattern
  :  imageFilePatterns
  <> siteFilePatterns

imageFilePatterns :: [FilePattern]
imageFilePatterns = [
    FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".svg"
  , FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".png"
  , FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".jpg"
  , FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".ico"
  ]

projectorFilePattern :: FilePattern
projectorFilePattern =
  FilePattern $ G.wildcard <> G.literal ".prj"

sassFilePattern :: FilePattern
sassFilePattern =
  FilePattern $ G.wildcard <> G.literal ".scss"

machinatorFilePattern :: FilePattern
machinatorFilePattern =
  FilePattern $ G.wildcard <> G.literal ".mcn"

siteFilePatterns :: [FilePattern]
siteFilePatterns = [
    FilePattern $ G.literal "README.md"
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
