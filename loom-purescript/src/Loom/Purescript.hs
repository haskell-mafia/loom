{-# LANGUAGE NoImplicitPrelude #-}
module Loom.Purescript (
    compilePurescript
  ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Language.PureScript as PS
import qualified Language.PureScript.Make as PM

import           P

import           System.FilePath (FilePath)
import           System.IO (IO)

compilePurescript ::
  [FilePath] ->
  FilePath ->
  IO (Either PS.MultipleErrors ())
compilePurescript input outputDir = do
  moduleFiles <- readInput input
  (result, warnings) <- PM.runMake defaultPurescriptOptions $ do
    ms <- PS.parseModulesFromFiles id moduleFiles
    let
      filePathMap =
        M.fromList . fmap (\(fp, PS.Module _ _ mn _ _) -> (mn, Right fp)) $ ms
    foreigns <- PM.inferForeignModules filePathMap
    let
       makeActions =
         PM.buildMakeActions outputDir filePathMap foreigns False
    PS.make makeActions . fmap snd $ ms
  -- Treat warnings as errors!
  pure $ case result of
    Left errors ->
      Left (errors <> warnings)
    Right _ ->
      if PS.nonEmpty warnings then
        Left warnings
      else
        pure ()

defaultPurescriptOptions :: PS.Options
defaultPurescriptOptions =
  PS.Options {
      PS.optionsNoTco = False
    , PS.optionsNoMagicDo = False
    , PS.optionsMain = Nothing
    , PS.optionsNoOptimizations = False
    , PS.optionsVerboseErrors = False
    , PS.optionsNoComments = True
    , PS.optionsSourceMaps = False
    }

-- FIX In newer versions of purescript this is Text not String
readInput :: [FilePath] -> IO [(FilePath, [Char])]
readInput inputFiles =
  forM inputFiles $ \inFile ->
    (,) inFile . T.unpack . T.decodeUtf8 <$> BS.readFile inFile
