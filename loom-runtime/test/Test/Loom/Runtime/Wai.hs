{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Loom.Runtime.Wai where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Loom.Runtime.Wai

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), joinPath, takeDirectory)
import           System.IO (IO)
import qualified System.IO.Temp as Temp

import           Test.QuickCheck.Jack (Gen, (===), (==>))
import qualified Test.QuickCheck.Jack as J
import           Test.QuickCheck.Instances ()

prop_wai_assets =
  J.forAll genFile $ \f1 ->
  J.forAll genFile $ \f2 ->
  fst f1 /= fst f2 ==>
  J.forAll (J.listOf genFile) $ \files ->
  testIO $ do
    let
      req f = Wai.defaultRequest {
          Wai.pathInfo = fst f
        }
      assets = Map.delete (fst f2) . fromList . fmap (first (T.intercalate "/")) $ f1 : files
      notFound _ resp = resp $ Wai.responseLBS HTTP.status404 [] ""
      app = assetsMiddleware assets notFound
    resp1 <- WT.runSession (WT.srequest $ WT.SRequest (req f1) "") app
    let
      req2 =
        (req f1) {
            Wai.requestHeaders =
              maybeToList . fmap ((,) "If-None-Match") . List.lookup "ETag" $ WT.simpleHeaders resp1
          }
    resp2 <- WT.runSession (WT.srequest $ WT.SRequest req2 "") app
    resp3 <- WT.runSession (WT.srequest $ WT.SRequest req2 "") app
    resp4 <- WT.runSession (WT.srequest $ WT.SRequest (req f2) "") app
    pure $ J.conjoin [
        WT.simpleStatus resp1 === HTTP.status200
      , WT.simpleStatus resp2 === HTTP.status304
      , WT.simpleStatus resp3 === HTTP.status304
      , WT.simpleStatus resp4 === HTTP.status404
      ]

prop_wai_assets_dev =
  J.forAll (J.noShrink genFile) $ \f1 ->
  J.forAll (J.noShrink genFile) $ \f2 ->
  J.forAll (J.noShrink J.arbitrary) $ \b ->
  fst f1 /= fst f2 ==>
  J.forAll (J.noShrink . J.listOf $ genFile) $ \files ->
  testIO . Temp.withTempDirectory "dist" "temp" $ \dir -> do
    let
      req f = Wai.defaultRequest {
          Wai.pathInfo = fst f
        }
      toFile f =
        dir </> joinPath (fmap T.unpack f)
      writeFile (f, b') = do
        createDirectoryIfMissing True . takeDirectory . toFile $ f
        BS.writeFile (toFile f) b'
      assets = Map.delete (fst f2) . fromListDev . fmap (\(f, _) -> (T.intercalate "/" f, toFile f)) $ f1 : files
      notFound _ resp = resp $ Wai.responseLBS HTTP.status404 [] ""
      app = assetsMiddlewareDev assets notFound
    mapM_ writeFile $ f1 : files
    resp1 <- WT.runSession (WT.srequest $ WT.SRequest (req f1) "") app
    -- Update the file after first request
    writeFile (fst f1, b)
    resp2 <- WT.runSession (WT.srequest $ WT.SRequest (req f1) "") app
    resp3 <- WT.runSession (WT.srequest $ WT.SRequest (req f2) "") app
    pure $ J.conjoin [
        WT.simpleStatus resp1 === HTTP.status200
      , WT.simpleBody resp1 === (Lazy.fromStrict . snd) f1
      , WT.simpleStatus resp2 === HTTP.status200
      , WT.simpleBody resp2 === Lazy.fromStrict b
      , WT.simpleStatus resp3 === HTTP.status404
      ]

genFile :: Gen ([Text], ByteString)
genFile = do
  (,)
    <$> (
      (<>)
        <$> J.listOfN 1 3 (T.pack <$> J.listOfN 1 10 (J.chooseChar ('a', 'z')))
        -- Make sure the file names can't conflict with another directory
        <*> (pure . flip mappend ".txt" . T.pack <$> J.listOfN 1 10 (J.chooseChar ('a', 'z')))
      )
    <*> J.arbitrary

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
