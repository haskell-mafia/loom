{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Loom.Wai.Assets where

import           Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Loom.Wai.Assets

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as WT

import           P

import           System.IO (IO)

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

genFile :: Gen ([Text], ByteString)
genFile = do
  (,)
    <$> J.listOf1 (T.pack <$> J.listOf1 (J.chooseChar ('a', 'z')))
    <*> J.arbitrary

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
