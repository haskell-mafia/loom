{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Loom.Core.Data (
    FilePattern (..)
  , Loom (..)
  , LoomResolved (..)
  , LoomTmp (..)
  , LoomHome (..)
  , LoomRoot (..)
  , LoomFile (..)
  , LoomName (..)
  , LoomConfig (..)
  , LoomConfigResolved (..)
  , Component (..)
  , ComponentFile (..)
  , PurescriptBundle (..)
  , ImageFile (..)
  , CssFile (..)
  , JsFile (..)
  , PursFile (..)
  , LoomSitePrefix (..)
  , AssetsPrefix (..)
  , Tarball (..)
  , Uri (..)
  , Sha1 (..)
  , GithubRepo (..)
  , GitRef (..)
  , NpmPackage (..)
  , NpmPackageVersion (..)
  , GithubDependency (..)
  , GithubDependencyType (..)
  , NpmDependency (..)
  , Bundle (..)
  , BundleName (..)
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
  , jsAssetPath
  , jsAssetFilePath
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
  , jsFilePattern
  , pursFilePattern
  , siteFilePatterns
  , findFiles
  , findFiles'
  ) where

import qualified Data.Text as T

import           P

import           System.FilePath (FilePath, (</>), makeRelative, normalise)
import qualified "Glob" System.FilePath.Glob as G
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

-- | Global directory used to cache loom binaries and dependencies.
--
-- Defaults to '$HOME/.loom'
newtype LoomHome =
  LoomHome {
      loomHomeFilePath :: FilePath
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
    , loomConfigJsPaths :: [FilePattern]
    , loomConfigJsBundles :: [Bundle]
    , loomConfigJsDepsNpm :: [NpmDependency]
    , loomConfigJsDepsGithub :: [GithubDependency]
    , loomConfigPurs :: PurescriptBundle FilePattern
    , loomConfigPursTest :: PurescriptBundle FilePattern
    } deriving (Eq, Show)

data LoomConfigResolved =
  LoomConfigResolved {
      loomConfigResolvedRoot :: LoomRoot
    , loomConfigResolvedName :: LoomName
    , loomConfigResolvedComponents :: [LoomFile]
    , loomConfigResolvedSass :: [LoomFile]
    , loomConfigResolvedJs :: [LoomFile]
    , loomConfigResolvedJsBundles :: [(BundleName, ([LoomFile], [LoomFile]))]
    , loomConfigResolvedJsDepsNpm :: [NpmDependency]
    , loomConfigResolvedJsDepsGithub :: [GithubDependency]
    , loomConfigResolvedPurs :: PurescriptBundle LoomFile
    , loomConfigResolvedPursTest :: PurescriptBundle LoomFile
    } deriving (Eq, Show)

data Component =
  Component {
      componentPath :: LoomFile
    , componentSassFiles :: [ComponentFile]
    , componentProjectorFiles :: [ComponentFile]
    , componentMachinatorFiles :: [ComponentFile]
    , componentImageFiles :: [ComponentFile]
    , componentJsFiles :: [ComponentFile]
    , componentPursFiles :: [ComponentFile]
    } deriving (Eq, Show)

data ComponentFile =
  ComponentFile {
      componentLoomFile :: LoomFile
    , componentRawFilePath :: FilePath
    } deriving (Eq, Show)

data PurescriptBundle a =
  PurescriptBundle {
      purescriptBundleFiles :: [a]
    , purescriptBundleDependencies :: [GithubDependency]
    , purescriptBundleMain :: Maybe Text
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

jsAssetPath :: LoomSitePrefix -> AssetsPrefix -> JsFile -> Text
jsAssetPath p apx j =
  loomSitePrefix p <> (T.pack . jsAssetFilePath apx) j

jsAssetFilePath :: AssetsPrefix -> JsFile -> FilePath
jsAssetFilePath apx jsf =
  assetsPrefix apx </> renderJsFile jsf

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
  c : cs >>= \(LoomConfig r _ comps sass js jsb _ _ purs pursTest) ->
    let
      rfp = FilePattern . G.literal . loomRootFilePath $ r
      rec ext = flip appendFilePattern (FilePattern (G.recursiveWildcard <> G.wildcard <> G.literal "." <> ext))
    in
      fmap (appendFilePattern rfp) . mconcat $ [
          comps >>= \cp ->
            fmap (appendFilePattern cp) componentFilePatterns
        , sass
        , fmap (rec (G.literal "js")) js
        , foldMap bundlePaths jsb
        , fmap (rec (G.literal "purs")) . purescriptBundleFiles $ purs
        , fmap (rec (G.literal "purs")) . purescriptBundleFiles $ pursTest
        ]

componentFilePatterns :: [FilePattern]
componentFilePatterns =
     projectorFilePattern
  :  machinatorFilePattern
  :  sassFilePattern
  :  jsFilePattern
  :  pursFilePattern
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
  FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".prj"

sassFilePattern :: FilePattern
sassFilePattern =
  FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".scss"

machinatorFilePattern :: FilePattern
machinatorFilePattern =
  FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".mcn"

jsFilePattern :: FilePattern
jsFilePattern =
  FilePattern $ G.recursiveWildcard <> G.literal "vanilla.js"

pursFilePattern :: FilePattern
pursFilePattern =
  FilePattern $ G.recursiveWildcard <> G.wildcard <> G.literal ".purs"

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

-- | An unstructured, unvalidated URI.
newtype Uri = Uri {
    unUri :: [Char]
  } deriving (Eq, Ord, Show)

-- | A SHA1 hash.
newtype Sha1 = Sha1 {
    unSha1 :: Text
  } deriving (Eq, Ord, Show)

-- | A path to a gzipped tarball.
newtype Tarball = Tarball {
    tarballFilePath :: FilePath
  } deriving (Eq, Ord, Show)

-- | A Github user/repo combination, e.g. @GithubRepo "purescript" "purescript-newtype"@.
data GithubRepo = GithubRepo {
    grUser :: Text
  , grRepo :: Text
  } deriving (Eq, Ord, Show)

-- | A git ref, e.g. '"tags/v2.0.0"' or '"bff4a0d"' or '"bff4a0db303949a5878a052c901849527c9390ff"'.
newtype GitRef = GitRef {
    unGitRef :: Text
  } deriving (Eq, Ord, Show)

-- | A NPM package name, e.g. '"react"'.
newtype NpmPackage = NpmPackage {
    unNpmPackage :: Text
  } deriving (Eq, Ord, Show)

-- | A NPM package version, e.g. "4.7.4".
newtype NpmPackageVersion = NpmPackageVersion {
    unNpmPackageVersion :: Text
  } deriving (Eq, Ord, Show)

-- | A fully specified Github dependency.
data GithubDependency = GithubDependency {
    ghdRepo :: GithubRepo
  , ghdRef :: GitRef
  , ghdSha1 :: Sha1
  , ghdType :: GithubDependencyType
  } deriving (Eq, Ord, Show)

data GithubDependencyType =
    GithubDependencyV1
  | GithubDependencyV2
  deriving (Eq, Ord, Show)

-- | A fully specified NPM dependency.
data NpmDependency = NpmDependency {
    ndPackage :: NpmPackage
  , ndPackageVersion :: NpmPackageVersion
  , ndSha1 :: Sha1
  } deriving (Eq, Ord, Show)

newtype JsFile = JsFile {
    renderJsFile :: FilePath
  } deriving (Eq, Ord, Show)

newtype PursFile = PursFile {
    renderPursFile :: FilePath
  } deriving (Eq, Ord, Show)

newtype BundleName = BundleName {
    unBundleName :: Text
  } deriving (Eq, Ord, Show)

data Bundle = Bundle {
    bundleName :: BundleName
  , bundleMain :: FilePattern
  , bundlePaths :: [FilePattern]
  } deriving (Eq, Show)
