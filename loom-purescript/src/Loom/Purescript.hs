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
  , compilePurescript
  , bundlePurescript
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Language.PureScript as PS
import qualified Language.PureScript.Bundle as PB
import qualified Language.PureScript.Make as PM

import           Loom.Core.Data
import qualified Loom.Fetch as LF
import           Loom.Fetch.HTTPS (HTTPSError, renderHTTPSError)
import           Loom.Fetch.HTTPS.Github (githubFetcher)

import           P

import qualified System.FilePath as FP
import qualified System.FilePath.Find as Find
import           System.IO (FilePath, IO, readFile)

import           X.Control.Monad.Trans.Either (EitherT, sequenceEitherT)


data PurescriptError =
    PurescriptFetchError [LF.FetchError HTTPSError]
  | PurescriptUnpackError [LF.FetchError ()]
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

fetchPurs :: LoomHome -> [GithubDependency] -> EitherT PurescriptError IO [LF.FetchedDependency]
fetchPurs home deps = do
  github <- liftIO githubFetcher
  let ghNamer = T.unpack . grRepo . ghdRepo
  firstT PurescriptFetchError $ LF.fetchDepsSha1 home github ghNamer ghdSha1 deps

unpackPurs :: PurescriptUnpackDir -> [LF.FetchedDependency] -> EitherT PurescriptError IO ()
unpackPurs (PurescriptUnpackDir out) deps = do
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

compilePurescript :: [FilePath] -> CodeGenDir -> IO (Either PS.MultipleErrors ())
compilePurescript input (CodeGenDir outputDir) = do
  moduleFiles <- readInput input
  (result, warnings) <- PM.runMake defaultPurescriptOptions $ do
    ms <- PS.parseModulesFromFiles id moduleFiles
    let
      filePathMap =
        M.fromList . fmap (\(fp, PS.Module _ _ mn _ _) -> (mn, Right fp)) $ ms
    foreigns <- PM.inferForeignModules filePathMap
    let
       makeActions =
         PM.buildMakeActions outputDir filePathMap foreigns False
    PS.make makeActions . fmap snd $ ms
  -- Treat warnings as errors!
  pure $ case result of
    Left errors ->
      Left (errors <> warnings)
    -- TODO serialising the externs would be wise, is used for incremental build
    Right _externs ->
      if PS.nonEmpty warnings then
        Left warnings
      else
        pure ()

bundlePurescript :: CodeGenDir -> IO JsBundle
bundlePurescript (CodeGenDir dir) = do
  -- List recursive
  files <- Find.find (Find.fileName Find./~? ".*") (Find.extension Find.==? ".js") dir
  let mkIdent fp =
        PB.ModuleIdentifier
          (FP.takeFileName (FP.takeDirectory (FP.makeRelative dir fp)))
          (case FP.takeFileName fp of
            "index.js" -> PB.Regular
            "foreign.js" -> PB.Foreign
            -- This is wrong but unlikely:
            _ -> PB.Regular)
  inputs <- for files $ \fp -> do
    -- TODO remove lazy io
    str <- readFile fp
    pure (mkIdent fp, str)
  either (fail . show) pure
    ((JsBundle . T.pack) <$> PB.bundle inputs (fmap fst inputs) Nothing "PS")

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

-- FIX In newer versions of purescript this is Text not String
readInput :: [FilePath] -> IO [(FilePath, [Char])]
readInput inputFiles =
  forM inputFiles $ \inFile ->
    (,) inFile . T.unpack . T.decodeUtf8 <$> BS.readFile inFile
