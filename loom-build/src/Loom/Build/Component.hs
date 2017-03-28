{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Component (
    ComponentError (..)
  , renderComponentError
  ---
  , resolveComponents
  , resolveComponent
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Loom.Core.Data

import           P

import           System.Directory (doesDirectoryExist)
import qualified System.FilePath.Glob as Glob
import           System.FilePath (makeRelative)
import           System.IO (FilePath, IO)

import           X.Control.Monad.Trans.Either (EitherT, left)

data ComponentError =
    ComponentMissing LoomFile
  deriving (Eq, Show)

renderComponentError :: ComponentError -> Text
renderComponentError ce =
  case ce of
    ComponentMissing f ->
      "Could not find component directory: " <> (T.pack . loomFilePath) f

-------------

resolveComponents :: [LoomFile] -> EitherT ComponentError IO [Component]
resolveComponents =
  mapM resolveComponent

resolveComponent :: LoomFile -> EitherT ComponentError IO Component
resolveComponent dir = do
  let
    dir' = loomFilePath dir
  unlessM (liftIO . doesDirectoryExist $ dir') $
    left $ ComponentMissing dir
  imgs <- liftIO (globDir imageFilePatterns dir')
  proj <- liftIO (globDir1 projectorFilePattern dir')
  mach <- liftIO (globDir1 machinatorFilePattern dir')
  sass <- liftIO (globDir1 sassFilePattern dir')
  let
    f f' = ComponentFile dir f'
    filterExamples = fmap f . filter (not . matches siteFilePatterns) . fmap (makeRelative dir')
  -- FIX More validation?
  pure $
    Component
      dir
      (filterExamples sass)
      (filterExamples proj)
      (filterExamples mach)
      (filterExamples imgs)

-------------

globDir :: [FilePattern] -> FilePath -> IO [FilePath]
globDir ps =
  fmap (fold . fst) . Glob.globDir (fmap unFilePattern ps)

globDir1 :: FilePattern -> FilePath -> IO [FilePath]
globDir1 (FilePattern p) =
  Glob.globDir1 p

matches :: [FilePattern] -> FilePath -> Bool
matches ps p =
  any (\(FilePattern pat) -> Glob.match pat p) ps
