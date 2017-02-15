{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Machinator (
    MachinatorError (..)
  , MachinatorHaskellError (..)
  , MachinatorInput (..)
  , MachinatorOutput (..)
  , ModuleName (..)
  , machinatorOutputModules
  , compileMachinator
  , generateMachinatorHaskell
  , renderMachinatorError
  , renderMachinatorHaskellError
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (handleIf)

import qualified Data.Char as Char
import           Data.List (stripPrefix)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Machinator.Core as MC
import qualified Machinator.Haskell as MH
import qualified Machinator.Haskell.Data.Types as MH

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (FilePath, (</>), takeDirectory, dropExtension, joinPath)
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
      machinatorOutputDefinitions :: Map.Map ModuleName [MC.Definition]
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
      n =
        (T.unpack . renderModuleName $ name) <> (fromMaybe f . stripPrefix root) f
    MC.Versioned _ (MC.DefinitionFile _ df) <-
      -- FIX We should be controlling the module name properly here
      hoistEither . first MachinatorError . MC.parseDefinitionFile n $ m
    pure (filePathToModuleName n, df)

  pure $ MachinatorOutput (d1 <> Map.fromList ds)

generateMachinatorHaskell :: FilePath -> [ModuleName] -> MachinatorOutput -> EitherT MachinatorHaskellError IO [FilePath]
generateMachinatorHaskell output imports (MachinatorOutput ds) = do
  hs <- hoistEither . first MachinatorHaskellError . MH.types MH.HaskellTypesV1 . with (Map.toList ds) $ \(n, d) ->
    (MC.DefinitionFile (moduleNameToFile "hs" n) d)
  liftIO . for hs $ \(f, t) -> do
    createDirectoryIfMissing True (output </> takeDirectory f)
    T.writeFile (output </> f) . hackImports imports $ t
    pure f

-- FIX Machinator needs to support his natively
-- https://github.com/ambiata/machinator/issues/12
hackImports :: [ModuleName] -> Text -> Text
hackImports imports =
  T.replace
    "where\n"
    ("where\n" <> (T.unlines . fmap (mappend "import " . renderModuleName)) imports)

machinatorOutputModules  :: MachinatorOutput -> [ModuleName]
machinatorOutputModules =
  Map.keys . machinatorOutputDefinitions

moduleNameToFile :: FilePath -> ModuleName -> FilePath
moduleNameToFile ext (ModuleName n) =
  (joinPath . fmap T.unpack . T.splitOn ".") n <> "." <> ext

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

-- FIX Remove from machinator
-- https://github.com/ambiata/machinator/blob/815e948b59a392d56c8589990279bc7ee464f12b/machinator-haskell/src/Machinator/Haskell/Scheme/Types.hs#L77
filePathToModuleName :: FilePath -> ModuleName
filePathToModuleName =
  ModuleName . T.pack . goUpper . dropExtension
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

-- FIX Common module?
readFileSafe :: MonadIO m => FilePath -> m (Maybe Text)
readFileSafe =
  liftIO . handleIf isDoesNotExistError (const $ pure Nothing) . fmap Just . T.readFile
