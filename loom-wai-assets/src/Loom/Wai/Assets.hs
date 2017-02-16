{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Wai.Assets (
    Middleware
  , Asset (..)
  , Assets
  , embedFile
  , assetsMiddleware
  , assetPaths
  , fromList
  ) where


import qualified Crypto.Hash as Crypto

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Char8 as Char8
import           Data.FileEmbed (embedFile)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import qualified Network.HTTP.Types as HTTP
import           Network.Mime (MimeType)
import qualified Network.Mime as Mime
import           Network.Wai (Middleware)
import qualified Network.Wai as Wai

import           P

import           System.FilePath (FilePath, normalise, splitDirectories)

data Asset =
  Asset {
      assetContents :: ByteString
    , assetMimeType :: MimeType
    , assetETag :: ByteString
    }

type Assets = Map [Text] Asset

assetsMiddleware :: Assets -> Middleware
assetsMiddleware assets app req resp =
  case Map.lookup (Wai.pathInfo req) assets of
    Nothing ->
      app req resp
    Just (Asset c mime etag) ->
      let
        cacheHeaders =
          [
              ("ETag", etag)
            ]
      in
        resp $
          if List.lookup "If-None-Match" (Wai.requestHeaders req) == Just etag then
            Wai.responseLBS HTTP.status304 cacheHeaders mempty
          else
            Wai.responseLBS HTTP.status200 (cacheHeaders <> [(HTTP.hContentType, mime)]) . Lazy.fromStrict $ c

assetPaths :: Assets -> [Text]
assetPaths =
  fmap (mappend "/" . T.intercalate "/") . Map.keys

fromList :: [(FilePath, ByteString)] -> Assets
fromList fs =
  Map.fromList . flip fmap fs $ \(fp, bs) ->
    (,)
      (fmap T.pack . splitDirectories . normalise $ fp)
      (Asset bs (Mime.defaultMimeLookup . T.pack $ fp) (calculateETag bs))

calculateETag :: ByteString -> ByteString
calculateETag b =
  Char8.pack . show $ (Crypto.hash b :: Crypto.Digest Crypto.MD5)
