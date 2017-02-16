{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Loom.Build.Assets where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import           Loom.Build.Assets

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.QuickCheck.Instances ()
import           Test.QuickCheck.Jack ((===))
import qualified Test.QuickCheck.Jack as J

prop_build_assets =
  J.forAll (Map.fromList <$> J.listOf1 (first (T.filter ((/=) '"')) <$> J.arbitrary)) $ \m ->
  J.forAll (J.listOf (J.oneOf [Left <$> J.elements (Map.toList m), Right <$> J.arbitrary])) $ \xs ->
    prefixCssImageAssetsRaw m (" " <> urls fst xs)
    ===
    " " <> urls snd xs

urls :: (a -> Text) -> [Either a Text] -> Text
urls f =
  mconcat . fmap (either (url . f) id)

url :: Text -> Text
url t =
  "url(\"" <> t <> "\")"

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunNormal
