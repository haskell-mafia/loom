{-# LANGUAGE NoImplicitPrelude #-}
module Loom.Purescript (
    PurescriptError (..)
  , renderPurescriptError
  , fetchPurs
  , compilePurescript
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Language.PureScript as PS
import qualified Language.PureScript.Make as PM

import           Loom.Core.Data
import qualified Loom.Fetch as LF
import           Loom.Fetch.HTTPS (HTTPSError, renderHTTPSError)
import           Loom.Fetch.HTTPS.Github (githubFetcher)

import           P

import qualified System.FilePath as FP
import           System.IO (FilePath, IO)

import           X.Control.Monad.Trans.Either (EitherT, sequenceEitherT)


data PurescriptError =
    PurescriptError
  | PurescriptFetchError [LF.FetchError HTTPSError]
  | PurescriptUnpackError [LF.FetchError ()]
  deriving (Eq, Ord, Show)

newtype PurescriptUnpackDir = PurescriptUnpackDir {
    unPurescriptUnpackDir :: FilePath
  } deriving (Eq, Ord, Show)

renderPurescriptError :: PurescriptError -> Text
renderPurescriptError =
  undefined

fetchPurs :: LoomHome -> [GithubDependency] -> EitherT PurescriptError IO [FetchedDependency]
fetchPurs home deps = do
  github <- liftIO githubFetcher
  let ghNamer = T.unpack . grRepo . ghdRepo
  firstT PurescriptFetchError $ LF.fetchDepsSha1 home github ghNamer ghdSha1 deps

unpackPurs :: PurescriptUnpackDir -> [FetchedDependency] -> EitherT PurescriptError IO ()
unpackPurs (PurescriptUnpackDir out) deps = do
  firstT PurescriptUnpackError . void . sequenceEitherT . with deps $ \dep ->
    firstT pure $ LF.unpackRenameDep (renameBaseDir (fetchedName dep)) dep out

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

compilePurescript :: [FilePath] -> FilePath -> IO (Either PS.MultipleErrors ())
compilePurescript input outputDir = do
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
    Right _ ->
      if PS.nonEmpty warnings then
        Left warnings
      else
        pure ()

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
