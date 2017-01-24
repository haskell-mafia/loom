{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Sass (
    Sass
  , SassError (..)
  , findSassOnPath
  , compileSass
  , renderSassError
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Loom.Process

import           P

import           System.FilePath (takeDirectory)
import           System.Directory (createDirectoryIfMissing)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

-- FIX Extract out loom-file, or alternative just switch back to the normal FilePath
type FilePath = T.Text

newtype Sass =
  Sass {
      sassPath :: FilePath
    }

data SassError =
    SassProcessError ProcessError
  deriving (Show)

findSassOnPath :: IO (Maybe Sass)
findSassOnPath =
  fmap Sass <$> verifyExecutable "sassc"

compileSass :: Sass -> FilePath -> FilePath -> EitherT SassError IO ()
compileSass sass input outFile = do
  liftIO . createDirectoryIfMissing True . takeDirectory . T.unpack $ outFile
  firstT SassProcessError . call (sassPath sass) . mconcat $ [
      [input]
    , [outFile]
    ]

renderSassError :: SassError -> Text
renderSassError se =
  case se of
    SassProcessError e ->
      "Error calling sassc: " <> renderProcessError e
