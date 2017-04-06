{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Js (
    JsError (..)
  , renderJsError
  , fetchJs
  , unpackJs
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           P

import           Loom.Core.Data
import           Loom.Fetch (FetchedDependency (..), FetchError, renderFetchError, fetchDepsSha1, unpackDep)
import           Loom.Fetch.HTTPS (HTTPSError, renderHTTPSError)
import           Loom.Fetch.HTTPS.Github (githubFetcher)
import           Loom.Fetch.HTTPS.Npm (npmFetcher)

import           System.FilePath (FilePath, (</>))
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, sequenceEitherT)


data JsError =
    JsFetchError [FetchError HTTPSError]
  | JsUnpackError [FetchError ()]
  deriving (Eq, Ord, Show)

renderJsError :: JsError -> Text
renderJsError je =
  case je of
    JsFetchError fes ->
      "Error while fetching JS:\n"
        <> T.unlines (fmap (renderFetchError renderHTTPSError) fes)
    JsUnpackError fes ->
      "Error unpacking JS:\n"
        <> T.unlines (fmap (renderFetchError (const "")) fes)

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

unpackJs :: LoomTmp -> [FetchedDependency] -> EitherT JsError IO ()
unpackJs tmp deps = do
  let out = jsDest tmp
  firstT JsUnpackError . void . sequenceEitherT . with deps $ \dep ->
    firstT pure $ unpackDep dep out

jsDest :: LoomTmp -> FilePath
jsDest (LoomTmp path) =
  path </> "js"
