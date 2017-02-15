{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Haskell (
    LoomHaskellError (..)
  , generateHaskell
  , generateAssetHaskell
  , generateCabal
  , renderLoomHaskellError
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Build.Data
import           Loom.Projector (ProjectorOutput)
import qualified Loom.Projector as Projector
import           Loom.Machinator (MachinatorHaskellError, MachinatorOutput)
import qualified Loom.Machinator as Machinator
import           Loom.Sass (CssFile (..))

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), FilePath, dropExtension, joinPath, takeDirectory)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

data LoomHaskellError =
    LoomHaskellMachinatorError MachinatorHaskellError
  deriving (Show)

generateHaskell ::
  FilePath ->
  LoomName ->
  CssFile ->
  ProjectorOutput ->
  MachinatorOutput ->
  [ImageFile] ->
  EitherT LoomHaskellError IO ()
generateHaskell output name outputCss po mo images = do
  void . firstT LoomHaskellMachinatorError $
    Machinator.generateMachinatorHaskell
      (output </> "src")
      (Machinator.ModuleName . Projector.unModuleName <$> Projector.requiredProjectorHaskellImports)
      mo
  void . liftIO $
    Projector.generateProjectorHaskell (output </> "src") po
  liftIO $
    generateAssetHaskell name output outputCss images
  liftIO $
    generateCabal name output mo po

generateAssetHaskell :: LoomName -> FilePath -> CssFile -> [ImageFile] -> IO ()
generateAssetHaskell name output css images = do
  let
    f = output </> "src" </> assetModulePath name
    q t = "\"" <> t <> "\""
  createDirectoryIfMissing True . takeDirectory $ f
  T.writeFile f $
    T.unlines [
        "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "module " <> renderAssetModuleName name <> " where"
      , ""
      , "import           Data.Text (Text)"
      , ""
      , "css :: [Text]"
      , "css = [" <> (q . T.pack . renderCssFile) css <> "]"
      , ""
      , "images :: [Text]"
      , "images ="
      , "  [  " <> (T.intercalate "\n    , " . fmap (q . T.pack . imageFilePath)) images
      , "    ]"
      ]

generateCabal ::
  LoomName ->
  FilePath ->
  MachinatorOutput ->
  ProjectorOutput ->
  IO ()
generateCabal name output mo po = do
  let
    n = (T.map (\c -> if Char.isAlphaNum c then Char.toLower c else '-') . renderLoomName) name <> "-loom"
  T.writeFile (output </> T.unpack n <> ".cabal") $
    T.unlines [
        "name:          " <> n
      , "version:       0.0.1"
      , "license:       AllRightsReserved"
      , "author:        Loom"
      , "maintainer:    Loom"
      , "synopsis:      synopsis"
      , "category:      Development"
      , "cabal-version: >= 1.8"
      , "build-type:    Simple"
      , "description:   description"
      , ""
      , "library"
      , "  hs-source-dirs: src"
      , "  build-depends:"
      , "      base                        >= 3          && < 5"
      , "    , transformers                >= 0.4        && < 6"
      , "    , text                        >= 1.1        && < 1.3"
      , "    , ambiata-projector-html-runtime"
      , ""
      , "  ghc-options: -Wall -O2"
      , ""
      , "  exposed-modules:"
      , T.unlines . fmap ((<>) "    ") . mconcat $ [
            [renderAssetModuleName name]
          , fmap Machinator.renderModuleName . Machinator.machinatorOutputModules $ mo
          , fmap Projector.unModuleName . Projector.projectorOutputModules $ po
          ]
      ]

assetModulePath :: LoomName -> FilePath
assetModulePath  =
  flip (<>) ".hs" . joinPath . fmap T.unpack . T.splitOn "." . renderAssetModuleName

renderAssetModuleName :: LoomName -> Text
renderAssetModuleName n =
  (filePathToModuleName . T.unpack . renderLoomName) n <> ".Assets"

filePathToModuleName :: FilePath -> Text
filePathToModuleName =
  T.pack . goUpper . dropExtension
  where
    goUpper [] = []
    goUpper (x:xs)
      | Char.isAlphaNum x = Char.toUpper x : go xs
      | otherwise = goUpper xs
    go [] = []
    go (x:xs)
      | x == '/' = '.' : goUpper xs
      | Char.isAlphaNum x = x : go xs
      | otherwise = goUpper xs

renderLoomHaskellError :: LoomHaskellError -> Text
renderLoomHaskellError he =
  case he of
    LoomHaskellMachinatorError e ->
      Machinator.renderMachinatorHaskellError e
