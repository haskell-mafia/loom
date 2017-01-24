{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Sass (
    Sass
  , SassIncludes (..)
  , findSassOnPath
  , buildSass

  , SassBuildError (..)
  , renderSassBuildError
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy

import           Loom.Cli.Build
import           Loom.Cli.File
import           Loom.Sass

import           P

import           System.IO (IO)

import qualified Text.Megaparsec as Mega
import           Text.Megaparsec.Text (Parser)

import           X.Control.Monad.Trans.Either (EitherT, left)


data SassIncludes =
  SassIncludes {
      sassMain :: FilePath
    , sassIncludes :: [FilePath]
    } deriving (Eq, Ord, Show)

data SassBuildError =
    SassError SassError
  | SassFileNotFound FilePath
    deriving (Show)

renderSassBuildError :: SassBuildError -> Text
renderSassBuildError = \case
  SassError e ->
    renderSassError e
  SassFileNotFound path ->
    "File not found: " <> path

buildSass :: Sass -> SassIncludes -> EitherT SassBuildError IO (Maybe FilePath)
buildSass sass (SassIncludes main includes) =
  doesFileExist main >>= \x -> case x of
    False ->
      pure Nothing
    True -> fmap Just $ do
      writeToFile "tmp/main.scss" $ \scss -> do
      writeToFile "tmp/main.css" $ \css -> do
        -- FIX Check if modified
        -- findFiles $ ["scss/**/*.scss"] <> modules ["scss"]

        writeUtf8 scss . Lazy.toStrict =<< expand includes main

        firstT SassError $ compileSass sass scss css

        -- FIX Autoprefix "last 2 version" "ie 10"
        -- https://github.com/postcss/autoprefixer
        -- FIX replaceAssetUrls
        -- FIX Source maps
        pure css

-- Sass doesn't support glob imports.
--
-- https://github.com/sass/sassc/issues/62
-- https://github.com/britco/node-sass-globbing
expand :: [FilePath] -> FilePath -> EitherT SassBuildError IO Lazy.Text
expand includes path = do
  msass <- readUtf8 path
  case msass of
    Nothing ->
      left $ SassFileNotFound path
    Just sass ->
      fmap Lazy.unlines $ traverse (expandLine includes path) $ T.lines sass

expandLine :: [FilePath] -> FilePath -> Text -> EitherT SassBuildError IO Lazy.Text
expandLine includes path txt =
  case Mega.runParser pImport (T.unpack path) txt of
    Left _ ->
      pure $ Lazy.fromStrict txt
    Right glob -> do
      xs <- fmap mconcat $ traverse (flip findFilesIn [glob]) includes
      fmap Lazy.unlines $ traverse (expand includes) xs

pImport :: Parser FilePath
pImport =
  let
    pre =
      Mega.string "@import" *> Mega.space *> Mega.char '"'

    post =
      Mega.char '"' <* Mega.space <* Mega.char ';'
  in
    T.pack <$> (pre *> many (Mega.noneOf ['"']) <* post)
