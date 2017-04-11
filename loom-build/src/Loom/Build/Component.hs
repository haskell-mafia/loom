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
  | GlobInvariant
  deriving (Eq, Show)

renderComponentError :: ComponentError -> Text
renderComponentError ce =
  case ce of
    ComponentMissing f ->
      "Could not find component directory: " <> (T.pack . loomFilePath) f
    GlobInvariant ->
      "BUG: Something went wrong with resolveComponent's globs"

-------------

resolveComponents :: [LoomFile] -> EitherT ComponentError IO [Component]
resolveComponents =
  mapM resolveComponent

resolveComponent :: LoomFile -> EitherT ComponentError IO Component
resolveComponent dir = do
  let
    dir' = loomFilePath dir
    pats =
        projectorFilePattern
      : machinatorFilePattern
      : sassFilePattern
      : jsFilePattern
      : pursFilePattern
      : imageFilePatterns
  unlessM (liftIO . doesDirectoryExist $ dir') $
    left $ ComponentMissing dir
  files <- liftIO (globDir pats dir')
  let
    f f' = ComponentFile dir (makeRelative dir' f')
    filterExamples = filter (not . matches siteFilePatterns . makeRelative dir')
  -- FIX More validation?
  case files of
    (proj : mach : sass : js : purs : imgs) ->
      pure $
        Component
          dir
          (fmap f sass)
          (fmap f (filterExamples proj))
          (fmap f mach)
          (fmap f (fold imgs))
          (fmap f js)
          (fmap f purs)
    _ ->
      left GlobInvariant

-------------

globDir :: [FilePattern] -> FilePath -> IO [[FilePath]]
globDir ps =
  fmap fst . Glob.globDir (fmap unFilePattern ps)

matches :: [FilePattern] -> FilePath -> Bool
matches ps p =
  any (\(FilePattern pat) -> Glob.match pat p) ps
