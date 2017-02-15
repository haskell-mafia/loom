{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Component (
    Component (..)
  , ComponentError (..)
  , ComponentFile
  , componentName
  , componentFilePath
  , renderComponentError
  ---
  , resolveComponents
  , resolveComponent
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.List (partition)
import qualified Data.Text as T

import           P

import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath ((</>), FilePath, takeBaseName, splitExtension)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left)

data Component =
  Component {
      componentPath :: FilePath
    , componentSassFiles :: [ComponentFile]
    , componentProjectorFiles :: [ComponentFile]
    , componentMachinatorFiles :: [ComponentFile]
    } deriving (Eq, Show)

data ComponentError =
    ComponentMissing FilePath
  deriving (Eq, Show)

newtype ComponentFile =
  ComponentFile FilePath
    deriving (Eq, Show)

componentName :: Component -> Text
componentName =
  T.pack . takeBaseName . componentPath

componentFilePath :: Component -> ComponentFile -> FilePath
componentFilePath c (ComponentFile f) =
  componentPath c </> f

renderComponentError :: ComponentError -> Text
renderComponentError ce =
  case ce of
    ComponentMissing f ->
      "Could not find component directory: " <> T.pack f

-------------

resolveComponents :: [FilePath] -> EitherT ComponentError IO [Component]
resolveComponents =
  mapM resolveComponent

resolveComponent :: FilePath -> EitherT ComponentError IO Component
resolveComponent dir = do
  unlessM (liftIO . doesDirectoryExist $ dir) $
    left $ ComponentMissing dir
  fs <- liftIO . fmap (filter (flip notElem [".", ".."])) . getDirectoryContents $ dir
  let
    (sass, r1) = partition (hasExtension "scss") fs
    (proj, r2) = partition (hasExtension "prj") r1
    (mach, _los) = partition (hasExtension "mcn") r2
  -- FIX More validation?
  pure $
    Component
      dir
      (fmap ComponentFile sass)
      (fmap ComponentFile proj)
      (fmap ComponentFile mach)

-------------

hasExtension :: [Char] -> FilePath -> Bool
hasExtension ext f =
  let
    (bn, fe) = splitExtension f
  in
    (not . null) bn && fe == "." <> ext
