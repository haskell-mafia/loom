{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Js (
    JsError (..)
  , renderJsError
  , fetchJs
  , fetchJsNpm
  , fetchJsGithub
  , JsUnpackDir (..)
  , unpackJs
  , JsModuleName (..)
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           P

import           Loom.Core.Data
import           Loom.Fetch (FetchedDependency (..), FetchError, renderFetchError, fetchDepsSha1, unpackRenameDep)
import           Loom.Fetch.HTTPS (HTTPSError, renderHTTPSError)
import           Loom.Fetch.HTTPS.Github (githubFetcher)
import           Loom.Fetch.HTTPS.Npm (npmFetcher)

import           System.FilePath (FilePath)
import qualified System.FilePath as FP
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, sequenceEitherT)


data JsError =
    JsFetchError [FetchError HTTPSError]
  | JsUnpackError [FetchError ()]
  | JsNodeMissing
  | JsNodeExitFailure Int Text Text
  | JsException Text
  | JsBrowserifyError Int Text
  deriving (Eq, Ord, Show)

newtype JsUnpackDir = JsUnpackDir {
    unJsUnpackDir :: FilePath
  } deriving (Eq, Ord, Show)

newtype JsModuleName = JsModuleName {
    unJsModuleName :: Text
  } deriving (Eq, Ord, Show)

renderJsError :: JsError -> Text
renderJsError je =
  case je of
    JsFetchError fes ->
      "Error while fetching JS:\n"
        <> T.unlines (fmap (renderFetchError renderHTTPSError) fes)
    JsUnpackError fes ->
      "Error unpacking JS:\n"
        <> T.unlines (fmap (renderFetchError (const "")) fes)
    JsNodeMissing ->
      T.unlines [
          "Could not locate 'node' executable on the PATH."
        , ""
        , "For OSX users try running the following first:"
        , " - brew install node"
        , ""
        , "For Arch users try running the following first:"
        , " - pacman -S node"
        , ""
        , "Otherwise please follow the build instructions here:"
        , " - https://github.com/nodejs/node"
        ]
    JsNodeExitFailure x out err ->
      T.unlines [
          "Error running 'node' (exit code " <> renderIntegral x <> "):"
        , out
        , err
        ]
    JsException t ->
      "Exception while processing JS:\n"
        <> t
    JsBrowserifyError x err ->
      "Error bundling JS (exit " <> renderIntegral x <> "):\n"
        <> err

fetchJs :: LoomHome -> [NpmDependency] -> [GithubDependency] -> EitherT JsError IO [FetchedDependency]
fetchJs home npms ghub = do
  (<>)
    <$> fetchJsNpm home npms
    <*> fetchJsGithub home ghub

fetchJsNpm :: LoomHome -> [NpmDependency] -> EitherT JsError IO [FetchedDependency]
fetchJsNpm home npms = do
  npm <- liftIO npmFetcher
  let npmNamer = T.unpack . unNpmPackage . ndPackage
  firstT JsFetchError $ fetchDepsSha1 home npm npmNamer ndSha1 npms

fetchJsGithub :: LoomHome -> [GithubDependency] -> EitherT JsError IO [FetchedDependency]
fetchJsGithub home grubs = do
  github <- liftIO githubFetcher
  let ghNamer = T.unpack . grRepo . ghdRepo
  firstT JsFetchError $ fetchDepsSha1 home github ghNamer ghdSha1 grubs

unpackJs :: JsUnpackDir -> [FetchedDependency] -> EitherT JsError IO ()
unpackJs (JsUnpackDir out) deps = do
  firstT JsUnpackError . void . sequenceEitherT . with deps $ \dep ->
    firstT pure $ unpackRenameDep (renameBaseDir (fetchedName dep)) dep out

renameBaseDir :: FilePath -> FilePath -> FilePath
renameBaseDir new fp =
  FP.joinPath $ case FP.splitPath fp of
    [] ->
      []
    (_:xs) ->
      (new:xs)
