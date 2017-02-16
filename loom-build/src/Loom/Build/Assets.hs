{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Assets (
    prefixCssImageAssets
  , prefixCssImageAssetsRaw
  , indexImageFiles
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Build.Data
import           Loom.Sass (CssFile, renderCssFile)

import           P

import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath (takeDirectory)
import           System.IO (IO)

prefixCssImageAssets :: AssetsPrefix -> [ImageFile] -> CssFile -> CssFile -> IO ()
prefixCssImageAssets apx images out inp = do
  e <- doesFileExist (renderCssFile inp)
  when e $ do
    bs <- T.readFile (renderCssFile inp)
    createDirectoryIfMissing True . takeDirectory . renderCssFile $ out
    T.writeFile (renderCssFile out) . prefixCssImageAssetsRaw (indexImageFiles apx images) $ bs

-- NOTE: Can't handle strings starting with 'url("'.
prefixCssImageAssetsRaw :: Map Text Text -> Text -> Text
prefixCssImageAssetsRaw images t =
  let
    replace v =
      case T.breakOn "\"" v of
         (h, r) ->
           "url(\"" <> Map.findWithDefault h h images <> r
  in
    case T.splitOn "url(\"" t of
      [] ->
        ""
      h : [] ->
        h
      h : ts ->
         T.concat $ h : fmap replace ts

indexImageFiles :: AssetsPrefix -> [ImageFile] -> Map Text Text
indexImageFiles apx images =
  Map.fromList . flip fmap images $ \i ->
    -- FIX Incorrectly using the component raw file path
    -- This will actually be fine until we have duplicate component/image paths
    (,)
      (T.pack . componentFilePathNoRoot $ imageComponentFile i)
      (imageAssetPath apx i)
