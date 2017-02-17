{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Wai.Assets (
    Middleware
  , Asset (..)
  , Assets
  , AssetDev (..)
  , AssetsDev
  , embedFile
  , assetsMiddleware
  , assetsMiddlewareDev
  , assetPaths
  , fromList
  , fromListDev
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
import qualified Data.Text.Encoding as T

import qualified Network.HTTP.Types as HTTP
import           Network.Mime (MimeType)
import qualified Network.Mime as Mime
import           Network.Wai (Middleware)
import qualified Network.Wai as Wai

import           P

import           System.FilePath (FilePath)

data Asset =
  Asset ByteString MimeType ByteString

data AssetDev =
  AssetDev MimeType FilePath

type Assets = Map [Text] Asset
type AssetsDev = Map [Text] AssetDev

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

assetsMiddlewareDev :: AssetsDev -> Middleware
assetsMiddlewareDev assets app req resp =
  case Map.lookup (Wai.pathInfo req) assets of
    Nothing ->
      app req resp
    Just (AssetDev mime f) ->
      resp $
        Wai.responseFile HTTP.status200 [(HTTP.hContentType, mime)] f Nothing

assetPaths :: Map [Text] a -> [Text]
assetPaths =
  fmap (mappend "/" . T.intercalate "/") . Map.keys

-- | Serve up a list of assets embedded in the haskell binary
fromList :: [(Text, ByteString)] -> Assets
fromList fs =
  Map.fromList . flip fmap fs $ \(fp, bs) ->
    (,)
      (HTTP.decodePathSegments . T.encodeUtf8 $ fp)
      (Asset bs (Mime.defaultMimeLookup fp) (calculateETag bs))

-- | Serve up a list of assets directly from disk without _any_ caching
fromListDev :: [(Text, FilePath)] -> AssetsDev
fromListDev fs =
  Map.fromList . flip fmap fs $ \(fp, f) ->
    (,)
      (HTTP.decodePathSegments . T.encodeUtf8 $ fp)
      (AssetDev (Mime.defaultMimeLookup fp) f)

calculateETag :: ByteString -> ByteString
calculateETag b =
  Char8.pack . show $ (Crypto.hash b :: Crypto.Digest Crypto.MD5)
