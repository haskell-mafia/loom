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

import           Data.List (partition)
import qualified Data.Text as T

import           Loom.Core.Data

import           P

import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath (FilePath, splitExtension)
import           System.IO (IO)

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
  unlessM (liftIO . doesDirectoryExist . loomFilePath $ dir) $
    left $ ComponentMissing dir
  fs <- liftIO . fmap (filter (flip notElem [".", ".."])) . getDirectoryContents . loomFilePath $ dir
  let
    (sass, r1) = partition (hasExtension "scss") fs
    (proj, r2) = partition (hasExtension "prj") r1
    (mach, r3) = partition (hasExtension "mcn") r2
    (svg, r4) = partition (hasExtension "svg") r3
    (png, r5) = partition (hasExtension "png") r4
    (jpg, _los) = partition (hasExtension "jpg") r5
    f f' = ComponentFile dir f'
  -- FIX More validation?
  pure $
    Component
      dir
      (fmap f sass)
      (fmap f proj)
      (fmap f mach)
      (fmap f $ svg <> png <> jpg)

-------------

hasExtension :: [Char] -> FilePath -> Bool
hasExtension ext f =
  let
    (bn, fe) = splitExtension f
  in
    (not . null) bn && fe == "." <> ext
