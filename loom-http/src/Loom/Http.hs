{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Http (
    LoomHttpBuild (..)
  , LoomHttpNotFound (..)
  , loomHttpApplication
  , loomHttpServeFilesMiddleware
  , loomHttpBuildMiddleware
  , loomHttpNotFound
  ) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Network.HTTP.Types as HTTP
import qualified Network.Mime as Mime
import           Network.Wai (Application, Middleware)
import qualified Network.Wai as Wai

import qualified System.Directory as Dir
import           System.FilePath (FilePath, (</>))
import           System.IO (IO)

import           P

-- | Represents the last (or current) build, which can block if in progress
newtype LoomHttpBuild =
  LoomHttpBuild (IO (Either Text ()))

-- | The html for
newtype LoomHttpNotFound =
  LoomHttpNotFound {
      renderLoomHttpNotFound :: Text
    }

-- | Create an "Wai.Application" that serves up the loom site from disk,
-- waits for builds in progress and returns any errors.
--
loomHttpApplication :: FilePath -> LoomHttpNotFound -> LoomHttpBuild -> Application
loomHttpApplication dist notFound build =
  -- Serve up files from disk
  loomHttpServeFilesMiddleware dist .

    -- File couldn't be found, wait for the current build result
    loomHttpBuildMiddleware build .

    -- This might look a little strange, but
    -- If the build is successful we want to load the file
    loomHttpServeFilesMiddleware dist .

    -- Nothing to see here
    loomHttpNotFound $ notFound

-- | Serve files from disk and defaults to "index.html" if the path is a directory
--
loomHttpServeFilesMiddleware :: FilePath -> Middleware
loomHttpServeFilesMiddleware dist app req resp = do
  let
    file' = dist </> (drop 1 . T.unpack . T.decodeUtf8 . Wai.rawPathInfo) req
  dir <- Dir.doesDirectoryExist file'
  let
    file = if dir then file' </> "index.html" else file'
  e <- Dir.doesFileExist file
  if e then
    resp $
      Wai.responseFile
        HTTP.status200
        [(HTTP.hContentType, Mime.defaultMimeLookup . T.pack $ file)]
        file
        Nothing
  else
    app req resp

-- | Wait for the current build and display errors if it failed.
-- Otherwise continue to the next request handler.
--
loomHttpBuildMiddleware :: LoomHttpBuild -> Middleware
loomHttpBuildMiddleware (LoomHttpBuild build) app req resp = do
  r <- build
  case r of
    Left e ->
      resp $
        Wai.responseBuilder
          HTTP.status500
          [(HTTP.hContentType, "text/html; charset=utf-8")]
          (Builder.byteString . T.encodeUtf8 $ e)
    Right () ->
      app req resp

-- | The end of the line - the final 404 when all else fails
--
loomHttpNotFound :: LoomHttpNotFound -> Application
loomHttpNotFound (LoomHttpNotFound body) _req resp =
  resp $
    Wai.responseBuilder
      HTTP.status404
      [(HTTP.hContentType, "text/html; charset=utf-8")]
      (Builder.byteString . T.encodeUtf8 $ body)
