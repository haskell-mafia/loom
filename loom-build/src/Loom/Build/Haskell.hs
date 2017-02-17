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

import           System.Directory (canonicalizePath, createDirectoryIfMissing)
import           System.FilePath ((</>), FilePath, dropExtension, joinPath, takeDirectory)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

data LoomHaskellError =
    LoomHaskellMachinatorError MachinatorHaskellError
  deriving (Show)

generateHaskell :: FilePath -> AssetsPrefix -> LoomResult -> EitherT LoomHaskellError IO ()
generateHaskell output apx (LoomResult name _ mo po outputCss images) = do
  void . firstT LoomHaskellMachinatorError $
    Machinator.generateMachinatorHaskell
      (output </> "src")
      (Machinator.ModuleName . Projector.unModuleName <$> Projector.requiredProjectorHaskellImports)
      mo
  void . liftIO $
    Projector.generateProjectorHaskell (output </> "src") po
  liftIO $
    generateAssetHaskell name output apx outputCss images
  liftIO $
    generateCabal name output mo po

generateAssetHaskell :: LoomName -> FilePath -> AssetsPrefix -> CssFile -> [ImageFile] -> IO ()
generateAssetHaskell name output apx css images = do
  let
    f = output </> "src" </> assetModulePath name
    q p t = "(\"" <> p <> "\", $(embedFile \"" <> T.pack t <> "\"))"
    q2 p t = "(\"" <> p <> "\", \"" <> T.pack t <> "\")"
  createDirectoryIfMissing True . takeDirectory $ f
  css' <- fmap ((,) (cssAssetPath apx css)) . canonicalizePath $ output </> renderCssFile css
  images' <- for images $ \i -> (,) (imageAssetPath apx i) <$> canonicalizePath (imageFilePath i)
  T.writeFile f $
    T.unlines [
        "{-# LANGUAGE CPP #-}"
      , "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE TemplateHaskell #-}"
      , "module " <> renderAssetModuleName name <> " where"
      , ""
      , "import           Data.Monoid ((<>))"
      , "import           Data.Text (Text)"
      , ""
      , "import           Loom.Wai.Assets"
      , ""
      , "#if CABAL"
      , "cssAssets :: Assets"
      , "cssAssets ="
      , "  fromList [" <> uncurry q css' <> "]"
      , "#else"
      , "cssAssets :: AssetsDev"
      , "cssAssets ="
      , "  fromListDev [" <> uncurry q2 css' <> "]"
      , "#endif"
      , ""
      , "css :: [Text]"
      , "css ="
      , "  assetPaths cssAssets"
      , ""
      , "#if CABAL"
      , "imagesAssets :: Assets"
      , "imagesAssets ="
      , "  fromList [  " <> (T.intercalate "\n    , " . fmap (uncurry q)) images'
      , "    ]"
      , "#else"
      , "imagesAssets :: AssetsDev"
      , "imagesAssets ="
      , "  fromListDev [  " <> (T.intercalate "\n    , " . fmap (uncurry q2)) images'
      , "    ]"
      , "#endif"
      , ""
      , "assetMiddleware :: Middleware"
      , "assetMiddleware ="
      , "#if CABAL"
      , "  assetsMiddleware"
      , "#else"
      , "  assetsMiddlewareDev"
      , "#endif"
      , "    (cssAssets <> imagesAssets)"
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
      , "    , ambiata-loom-wai-assets"
      , "    , ambiata-projector-html-runtime"
      , ""
      , "  ghc-options: -Wall -O2 -DCABAL=1"
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
