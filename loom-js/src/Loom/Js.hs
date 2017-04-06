{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Js (
    LoomJsError (..)
  , renderLoomJsError
  , fetchJs
  , unpackJs
  ) where


import           Control.Monad.IO.Class (liftIO)

import           P

import           Loom.Core.Data
import           Loom.Fetch
import           Loom.Fetch.HTTPS.Github (githubFetcher)

import           X.Control.Monad.Trans.Either (EitherT)


data LoomJsError =
    LoomJsError
  deriving (Eq, Ord, Show)

renderLoomJsError :: LoomJsError -> Text
renderLoomJsError je =
  case je of
    LoomJsError ->
      "something went wrong pal"

fetchJs :: LoomHome -> [NpmDependency] -> [GithubDependency] -> EitherT LoomJsError IO [FetchedDependency]
fetchJs home npms ghub = do
  github <- githubFetcher

unpackJs :: LoomTmp -> [FetchedDependency] -> EitherT LoomJsError IO ()
unpackJs tmp deps =
  undefined
