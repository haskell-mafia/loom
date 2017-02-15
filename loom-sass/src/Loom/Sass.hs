{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Sass (
    Sass
  , SassError (..)
  , SassStyle (..)
  , CssFile (..)
  , findSassOnPath
  , compileSass
  , compileSassFile
  , renderSassError
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Process

import           P

import           System.FilePath ((</>), FilePath, takeDirectory, takeBaseName)
import           System.Directory (createDirectoryIfMissing)
import           System.IO (IO)
import qualified System.IO.Temp as Temp

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, runEitherT)

newtype Sass =
  Sass {
      sassPath :: FilePath
    }

data SassError =
    SassProcessError ProcessError
  deriving (Show)

data SassStyle =
    SassNested
  | SassExpanded
  | SassCompact
  | SassCompressed
  deriving (Bounded, Enum, Eq, Show)

newtype CssFile =
  CssFile {
      renderCssFile :: FilePath
    } deriving (Eq, Show)

findSassOnPath :: IO (Maybe Sass)
findSassOnPath =
  fmap Sass <$> verifyExecutable "sassc"

-- NOTE: `sassc` only expects a single input file, which in turn depends on other sass files.
-- For multiple inputs we write out a single sass with those inports and use that instead.
-- This is far less than ideal, but is forced upon us by sass.
compileSass :: Sass -> SassStyle -> CssFile -> [FilePath] -> EitherT SassError IO ()
compileSass sass style (CssFile outFile) inputs = do
  liftIO . createDirectoryIfMissing True . takeDirectory $ outFile
  newEitherT . Temp.withTempDirectory (takeDirectory outFile) "loom-tmp" $ \tempDir ->
    runEitherT $ do
      let
        inputFile = tempDir </> takeBaseName outFile <> ".scss"
      liftIO . T.writeFile inputFile . generateSassFile $ inputs
      compileSassFile sass style (CssFile outFile) inputFile

compileSassFile :: Sass -> SassStyle -> CssFile -> FilePath -> EitherT SassError IO ()
compileSassFile sass style (CssFile outFile) input = do
  liftIO . createDirectoryIfMissing True . takeDirectory $ outFile
  firstT SassProcessError . call (sassPath sass) . mconcat $ [
      [T.pack input]
    , ["--style", renderSassStyle style]
    , [T.pack outFile]
    ]

generateSassFile :: [FilePath] -> Text
generateSassFile inputs =
  T.unlines . with inputs $ \f ->
    mconcat [
        "@import "
      , "'"
      , T.pack f
      , "';"
      ]

renderSassError :: SassError -> Text
renderSassError se =
  case se of
    SassProcessError e ->
      "Error calling sassc: " <> renderProcessError e

renderSassStyle :: SassStyle -> Text
renderSassStyle ss =
  case ss of
    SassNested ->
      "nested"
    SassExpanded ->
      "expanded"
    SassCompact ->
      "compact"
    SassCompressed ->
      "compressed"
