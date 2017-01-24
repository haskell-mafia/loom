{-# LANGUAGE NoImplicitPrelude #-}
module Loom.Purescript (
    compilePurescript
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Language.PureScript as P
import qualified Language.PureScript.Make as PM

import           P

import           System.FilePath (FilePath)
import           System.IO (IO)

compilePurescript ::
  [FilePath] ->
  FilePath ->
  IO (Either P.MultipleErrors ())
compilePurescript input outputDir = do
  moduleFiles <- readInput input
  (result, warnings) <- PM.runMake defaultPurescriptOptions $ do
    ms <- P.parseModulesFromFiles id moduleFiles
    let
      filePathMap =
        M.fromList . fmap (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) $ ms
    foreigns <- PM.inferForeignModules filePathMap
    let
       makeActions =
         PM.buildMakeActions outputDir filePathMap foreigns False
    P.make makeActions . fmap snd $ ms
  -- Treat warnings as errors!
  pure $ case result of
    Left errors ->
      Left (errors <> warnings)
    Right _ ->
      if P.nonEmpty warnings then
        Left warnings
      else
        pure ()

defaultPurescriptOptions :: P.Options
defaultPurescriptOptions =
  P.Options {
      P.optionsNoTco = False
    , P.optionsNoMagicDo = False
    , P.optionsMain = Nothing
    , P.optionsNoOptimizations = False
    , P.optionsVerboseErrors = False
    , P.optionsNoComments = True
    , P.optionsSourceMaps = False
    }

-- FIX In newer versions of purescript this is Text not String
readInput :: [FilePath] -> IO [(FilePath, [Char])]
readInput inputFiles =
  forM inputFiles $ \inFile ->
    (,) inFile . T.unpack . T.decodeUtf8 <$> BS.readFile inFile
