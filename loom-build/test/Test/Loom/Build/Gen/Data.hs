{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Loom.Build.Gen.Data (
    genFilePattern
  ) where

import           Loom.Build.Data

import           P

import           Test.QuickCheck.Jack (Gen)
import qualified Test.QuickCheck.Jack as J

genFilePattern :: Gen FilePattern
genFilePattern =
  J.oneOfRec
    [genFilePatternSegment]
    [appendFilePattern <$> genFilePatternSegment <*> genFilePattern]

genFilePatternSegment :: Gen FilePattern
genFilePatternSegment =
  J.justOf $
    rightToMaybe . compileFilePattern
      <$> J.elements ["module", "component", "src", "app", "*"]
