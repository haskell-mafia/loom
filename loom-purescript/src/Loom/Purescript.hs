{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Purescript (
    PurescriptError (..)
  , renderPurescriptError
  , fetchPurs
  , PurescriptUnpackDir (..)
  , unpackPurs
  , CodeGenDir (..)
  , JsBundle (..)
  , MakeOutput (..)
  , compile
  , compilePurescript
  , bundlePurescript
  , expandPursPath
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Language.PureScript as PS
import qualified Language.PureScript.Bundle as PB
import qualified Language.PureScript.Errors as PE
import qualified Language.PureScript.Make as PM

import           Loom.Core.Data
import qualified Loom.Fetch as LF
import           Loom.Fetch.HTTPS (HTTPSError, renderHTTPSError)
import           Loom.Fetch.HTTPS.Github (githubFetcher)

import           P

import qualified System.Directory as D
import           System.FilePath ((</>))
import qualified System.FilePath as FP
import qualified System.FilePath.Find as Find
import           System.IO (FilePath, IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, sequenceEitherT)


data PurescriptError =
    PurescriptFetchError [LF.FetchError HTTPSError]
  | PurescriptUnpackError [LF.FetchError ()]
  | PurescriptMakeError Text
  | PurescriptBundleError Text
  deriving (Eq, Ord, Show)

newtype PurescriptUnpackDir = PurescriptUnpackDir {
    unPurescriptUnpackDir :: FilePath
  } deriving (Eq, Ord, Show)

renderPurescriptError :: PurescriptError -> Text
renderPurescriptError pe =
  case pe of
    PurescriptFetchError fes ->
      "Error while fetching Purescript dependencies:\n"
        <> T.unlines (fmap (LF.renderFetchError renderHTTPSError) fes)
    PurescriptUnpackError fes ->
      "Error unpacking Purescript dependencies:\n"
        <> T.unlines (fmap (LF.renderFetchError (const "")) fes)
    PurescriptMakeError t ->
      "Error compiling Purescript:\n"
        <> t
    PurescriptBundleError t ->
      "Error bundling Purescript:\n"
        <> t

fetchPurs :: LoomHome -> [GithubDependency] -> EitherT PurescriptError IO [LF.FetchedDependency]
fetchPurs home deps = do
  github <- liftIO githubFetcher
  let ghNamer = T.unpack . grRepo . ghdRepo
  firstT PurescriptFetchError $ LF.fetchDepsSha1 home github ghNamer ghdSha1 deps

unpackPurs :: PurescriptUnpackDir -> [LF.FetchedDependency] -> EitherT PurescriptError IO ()
unpackPurs (PurescriptUnpackDir out) deps = do
  liftIO $ D.createDirectoryIfMissing True out
  firstT PurescriptUnpackError . void . sequenceEitherT . with deps $ \dep ->
    firstT pure $ LF.unpackRenameDep (renameBaseDir (LF.fetchedName dep)) dep out

-- Intended use:
-- renameBaseDir "quux" "bar/foo" -> "quux/foo"
renameBaseDir :: FilePath -> FilePath -> FilePath
renameBaseDir new fp =
  FP.joinPath $ case FP.splitPath fp of
    [] ->
      []
    (_:xs) ->
      (new:xs)

-- -----------------------------------------------------------------------------

newtype CodeGenDir = CodeGenDir {
    unCodeGenDir :: FilePath
  } deriving (Eq, Ord, Show)

newtype JsBundle = JsBundle {
    unJsBundle :: Text
  } deriving (Eq, Ord, Show)

data ErrorLevel =
    WarningsAreErrors
  | IgnoreWarnings

data MakeOutput = MakeOutput {
    makeOutputDeps :: [PS.ModuleName]
  , makeOutputSrc :: [PS.ModuleName]
  } deriving (Show)

compile :: PurescriptUnpackDir -> [FilePath] -> CodeGenDir -> EitherT PurescriptError IO MakeOutput
compile depsDir input out = do
  deps <- liftIO $ findSrcPurs depsDir
  dext <- fmap PS.efModuleName <$> compilePurescript IgnoreWarnings deps out
  mext <- fmap PS.efModuleName <$> compilePurescript WarningsAreErrors (deps <> input) out
  pure MakeOutput {
      makeOutputDeps = dext
    , makeOutputSrc = mext L.\\ dext
    }

compilePurescript :: ErrorLevel -> [FilePath] -> CodeGenDir -> EitherT PurescriptError IO [PS.ExternsFile]
compilePurescript err input (CodeGenDir outputDir) = do
  liftIO $ D.createDirectoryIfMissing True outputDir
  moduleFiles <- liftIO $ readInput input
  (result, warnings) <- liftIO . PM.runMake defaultPurescriptOptions $ do
    ms <- PS.parseModulesFromFiles id moduleFiles
    let
      filePathMap =
        M.fromList . fmap (\(fp, PS.Module _ _ mn _ _) -> (mn, Right fp)) $ ms
    foreigns <- PM.inferForeignModules filePathMap
    let
       makeActions =
         PM.buildMakeActions outputDir filePathMap foreigns False
    PS.make makeActions . fmap snd $ ms
  hoistEither . first packMultipleMakeErrors $
    case err of
      WarningsAreErrors ->
        -- Treat warnings as errors!
        case result of
          Left errors ->
            Left ((errors <> warnings))
          -- TODO serialising the externs would be wise, is used for incremental build
          Right externs ->
            if PS.nonEmpty warnings then
              Left warnings
            else
              pure externs
      IgnoreWarnings ->
        result

bundlePurescript :: CodeGenDir -> MakeOutput -> EitherT PurescriptError IO JsBundle
bundlePurescript dir (MakeOutput deps src) = do
  bundlePurescript' dir (deps <> src) src

bundlePurescript' ::
     CodeGenDir
  -> [PS.ModuleName]
  -> [PS.ModuleName]
  -> EitherT PurescriptError IO JsBundle
bundlePurescript' (CodeGenDir dir) alljs entries = do
  files <- S.fromList <$> liftIO (findByExtension "js" dir)
  let mkIdents mn =
        let ms = PS.runModuleName mn
            mi = PB.ModuleIdentifier ms
            index = dir </> ms </> "index.js"
            forrn = dir </> ms </> "foreign.js"
        in fold [
            if S.member index files then [(index, mi PB.Regular)] else []
          , if S.member forrn files then [(forrn, mi PB.Foreign)] else []
          ]
      allIdents = foldMap mkIdents alljs
      entryPts = fmap snd (foldMap mkIdents entries)
  inputs <- (L.zip (fmap snd allIdents) . fmap snd) <$> liftIO (readInput (fmap fst allIdents))
  t <- hoistEither . first packBundleError $
    (T.pack <$> PB.bundle inputs entryPts Nothing "PS")
  pure (JsBundle (T.unlines [t, "module.exports = PS;"]))

defaultPurescriptOptions :: PS.Options
defaultPurescriptOptions =
  PS.Options {
      PS.optionsNoTco = False
    , PS.optionsNoMagicDo = False
    , PS.optionsMain = Nothing
    , PS.optionsNoOptimizations = False
    , PS.optionsVerboseErrors = False
    , PS.optionsNoComments = True
    , PS.optionsSourceMaps = False
    }

packBundleError :: PB.ErrorMessage -> PurescriptError
packBundleError =
  PurescriptBundleError . T.unlines . fmap T.pack . PB.printErrorMessage

packMultipleMakeErrors :: PS.MultipleErrors -> PurescriptError
packMultipleMakeErrors =
  PurescriptMakeError . T.pack . PE.prettyPrintMultipleErrors PE.defaultPPEOptions

-- FIX In newer versions of purescript this is Text not String
readInput :: [FilePath] -> IO [(FilePath, [Char])]
readInput inputFiles =
  forM inputFiles $ \inFile ->
    (,) inFile . T.unpack . T.decodeUtf8 <$> BS.readFile inFile

-- | This pretty much does '*/src/**/*.purs'.
findSrcPurs :: PurescriptUnpackDir -> IO [FilePath]
findSrcPurs (PurescriptUnpackDir depsDir) =
  Find.find recur (Find.extension Find.==? ".purs") depsDir
  where
    recur = do
      d <- Find.depth
      f <- Find.fileName
      let notHidden = pure f Find./~? ".*"
          depthOne = pure (d <= 1)
          isSrc = pure (d == 2 && FP.takeFileName f == "src")
          depthN = pure (d > 2)
      notHidden Find.&&? (depthOne Find.||? isSrc Find.||? depthN)

findByExtension :: [Char] -> FilePath -> IO [FilePath]
findByExtension ext =
  Find.find (Find.fileName Find./~? ".*") (Find.extension Find.==? ('.' : ext))

expandPursPath :: FilePath -> IO [FilePath]
expandPursPath path =
  findByExtension "purs" path
