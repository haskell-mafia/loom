{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Sass (
    Sass
  , SassError (..)
  , SassStyle (..)
  , findSassOnPath
  , compileSass
  , compileSassFile
  , renderSassError
  , commonPrefix
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.List (stripPrefix)
import qualified Data.Text as T

import           Loom.Process

import           P

import           System.FilePath ((</>), FilePath, takeDirectory, takeBaseName, takeFileName)
import           System.Directory (createDirectoryIfMissing)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

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

findSassOnPath :: IO (Maybe Sass)
findSassOnPath =
  fmap Sass <$> verifyExecutable "sassc"

-- Returns the generated files _relative_ path from the output directory
compileSass :: Sass -> SassStyle -> [FilePath] -> FilePath -> EitherT SassError IO [FilePath]
compileSass sass style inputs outDir =
  for inputs $ \input -> do
    let
      (base, dist, rest) = commonPrefix outDir input
      file = takeDirectory rest </> takeBaseName input <> ".css"
      fileD = dist </> file
      outputFile = maybe fileD (</> fileD) base
    compileSassFile sass style input outputFile
    pure file

compileSassFile :: Sass -> SassStyle -> FilePath -> FilePath -> EitherT SassError IO ()
compileSassFile sass style input outFile = do
  liftIO . createDirectoryIfMissing True . takeDirectory $ outFile
  firstT SassProcessError . call (sassPath sass) . mconcat $ [
      [T.pack input]
    , ["--style", renderSassStyle style]
    , [T.pack outFile]
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

-- | Takes two paths and calculates the longest common prefix
--
-- @
-- commonPrefix "a/b/c" "a/b/d" == (Just "a/b", "c", "d")
-- commonPrefix "a/b/c" "a/d/e" == (Just "a", "b/c", "d/e")
-- commonPrefix "x/b/c" "y/d/e" == (Nothing, "x/b/c", "y/d/e")
-- @
commonPrefix :: FilePath -> FilePath -> (Maybe FilePath, FilePath, FilePath)
commonPrefix orig =
  let
    go kk ff pk =
      if takeDirectory kk == kk then
        (Nothing, orig, pk)
      else case stripPrefix (kk <> "/") pk of
        Just p ->
         (Just kk, ff, p)
        Nothing ->
          go (takeDirectory kk) (takeFileName kk </> ff) pk
  in
    go orig ""
