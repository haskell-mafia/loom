{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Machinator (
    MachinatorError (..)
  , MachinatorHaskellError (..)
  , MachinatorInput (..)
  , MachinatorOutput (..)
  , ModuleName (..)
  , MC.Definition
  , compileMachinator
  , renderMachinatorError
  , renderMachinatorHaskellError
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (handleIf)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Machinator.Core as MC
import qualified Machinator.Haskell as MH
import qualified Machinator.Haskell.Data.Types as MH

import           P

import           System.FilePath (FilePath, (</>), makeRelative)
import           System.IO (IO)
import           System.IO.Error (isDoesNotExistError)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, hoistEither)

data MachinatorError =
    MachinatorFileMissing FilePath
  | MachinatorError MC.MachinatorError
    deriving (Eq, Show)

data MachinatorHaskellError =
    MachinatorHaskellError MH.HaskellTypesError
    deriving (Eq, Show)

-- FIX Should come from Machinator
-- https://github.com/ambiata/machinator/blob/master/machinator-haskell/src/Machinator/Haskell/Scheme/Types.hs#L65
newtype ModuleName =
  ModuleName {
      renderModuleName :: Text
    } deriving (Eq, Ord, Show)

data MachinatorInput =
  MachinatorInput {
      machinatorInputName :: ModuleName
    , machinatorInputRoot :: FilePath
    , machinatorInputFiles :: [FilePath]
    }

data MachinatorOutput =
  MachinatorOutput {
      machinatorOutputDefinitions :: Map.Map FilePath [MC.Definition]
    }

instance Monoid MachinatorOutput where
  mempty =
    MachinatorOutput mempty
  mappend (MachinatorOutput d1) (MachinatorOutput d2) =
    MachinatorOutput (d1 <> d2)

compileMachinator :: [MachinatorInput] -> EitherT MachinatorError IO MachinatorOutput
compileMachinator =
  foldM compileMachinatorIncremental mempty

compileMachinatorIncremental ::
  MachinatorOutput ->
  MachinatorInput ->
  EitherT MachinatorError IO MachinatorOutput
compileMachinatorIncremental (MachinatorOutput d1) (MachinatorInput name root inputs) = do
  ds <- for inputs $ \f -> do
    m <- newEitherT . fmap (maybeToRight (MachinatorFileMissing f)) . readFileSafe $ f
    let
      -- FIX this name is used only for error locations, but it's quite wrong
      n =
        (T.unpack . renderModuleName) name </> makeRelative root f
    MC.Versioned _ (MC.DefinitionFile _ df) <-
      -- FIX We should be controlling the module name properly here
      hoistEither . first MachinatorError . MC.parseDefinitionFile n $ m
    pure (f, df)

  pure $ MachinatorOutput (d1 <> Map.fromList ds)

renderMachinatorError :: MachinatorError -> Text
renderMachinatorError pe =
  case pe of
    MachinatorFileMissing f ->
      "Could not find file: " <> T.pack f
    MachinatorError me ->
      "Machinator build errors:\n" <> MC.renderMachinatorError me

renderMachinatorHaskellError :: MachinatorHaskellError -> Text
renderMachinatorHaskellError pe =
  case pe of
    MachinatorHaskellError me ->
      "Machinator haskell errors:\n" <> MH.renderHaskellTypesError me

-- FIX Common module?
readFileSafe :: MonadIO m => FilePath -> m (Maybe Text)
readFileSafe =
  liftIO . handleIf isDoesNotExistError (const $ pure Nothing) . fmap Just . T.readFile
