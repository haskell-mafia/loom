{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Projector (
    ProjectorError (..)
  , ProjectorInput (..)
  , ProjectorOutput
  , MachinatorModules
  , projectorOutputModules
  , DataModuleName (..)
  , ModuleName (..)
  , compileProjector
  , generateProjectorHaskell
  , moduleNameFromFile
  , requiredProjectorHaskellImports
  , renderProjectorError
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (handleIf)

import           Data.List (stripPrefix)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Machinator.Core as MC

import           P

import           Projector.Html (DataModuleName (..), ModuleName (..))
import qualified Projector.Html as Projector
import qualified Projector.Html.Data.Backend as Projector

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (FilePath, (</>), takeDirectory, joinPath)
import           System.IO (IO)
import           System.IO.Error (isDoesNotExistError)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, hoistEither)

data ProjectorError =
    ProjectorFileMissing FilePath
  | ProjectorError [Projector.HtmlError]
    deriving (Eq, Show)

data ProjectorInput =
  ProjectorInput {
      projectorModuleName :: ModuleName
    , projectorModuleRoot :: FilePath
    , projectorModuleTemplates :: [FilePath]
    } deriving (Show)

data ProjectorOutput =
  ProjectorOutput {
      -- FIX Expose the correct HtmlModules type
      _projectorOutputArtefacts :: Projector.BuildArtefacts
    }

instance Monoid ProjectorOutput where
  mempty =
    ProjectorOutput (Projector.BuildArtefacts mempty mempty)
  mappend (ProjectorOutput (Projector.BuildArtefacts _ d1)) (ProjectorOutput (Projector.BuildArtefacts _ d2)) =
    ProjectorOutput (Projector.BuildArtefacts mempty (d1 <> d2))

projectorOutputModules :: ProjectorOutput -> [ModuleName]
projectorOutputModules (ProjectorOutput (Projector.BuildArtefacts _ ms)) =
  Map.keys ms

-- FIX Should be in machinator
type MachinatorModules = Map.Map Projector.DataModuleName [MC.Definition]

compileProjector ::
  MachinatorModules ->
  [ProjectorInput] ->
  EitherT ProjectorError IO ProjectorOutput
compileProjector udts =
  foldM (compileProjectorIncremental udts) mempty

compileProjectorIncremental ::
  MachinatorModules ->
  ProjectorOutput ->
  ProjectorInput ->
  EitherT ProjectorError IO ProjectorOutput
compileProjectorIncremental
  udts
  (ProjectorOutput (Projector.BuildArtefacts _ oh1))
  (ProjectorInput prefix root inputs) = do

  templates <- for inputs $ \input ->
    fmap ((,) (moduleNameFromFile . fromMaybe input . stripPrefix root $ input)) .
      newEitherT . fmap (maybeToRight (ProjectorFileMissing input)) . readFileSafe $
        input
  Projector.BuildArtefacts _ oh2 <- firstT ProjectorError . hoistEither $
    Projector.runBuildIncremental
      (Projector.Build
        -- FIX Remove backend compiling from runBuildIncremental
        Nothing
        (Projector.moduleNamerSimple (Just prefix))
        (Map.keys udts)
        )
      (Projector.UserDataTypes . join . Map.elems $ udts)
      oh1
      (Projector.RawTemplates . fmap (first (moduleNameToFile "prj")) $ templates)
  pure $ ProjectorOutput (Projector.BuildArtefacts mempty (oh1 <> oh2))

generateProjectorHaskell :: FilePath -> ProjectorOutput -> IO [FilePath]
generateProjectorHaskell output (ProjectorOutput (Projector.BuildArtefacts _ oh2)) =
  for (Map.toList oh2) $ \(n, m) -> do
    let
      -- TODO validateModules
      -- https://github.com/ambiata/projector/blob/master/projector-html/src/Projector/Html.hs#L216
      (_, t) = Projector.codeGenModule Projector.Haskell n m
      f = moduleNameToFile "hs" n
    createDirectoryIfMissing True (output </> takeDirectory f)
    T.writeFile (output </> f) t
    pure f

moduleNameFromFile :: FilePath -> ModuleName
moduleNameFromFile =
  Projector.pathToModuleName (Projector.moduleNamerSimple Nothing)

moduleNameToFile :: FilePath -> ModuleName -> FilePath
moduleNameToFile ext (ModuleName n) =
  (joinPath . fmap T.unpack . T.splitOn ".") n <> "." <> ext

requiredProjectorHaskellImports :: [ModuleName]
requiredProjectorHaskellImports =
  [
      Projector.htmlRuntime
    ]

renderProjectorError :: ProjectorError -> Text
renderProjectorError pe =
  case pe of
    ProjectorFileMissing f ->
      "Could not find file: " <> T.pack f
    ProjectorError es ->
      "Projector build errors:\n" <> T.unlines (fmap Projector.renderHtmlError es)

-- FIX Common module?
readFileSafe :: MonadIO m => FilePath -> m (Maybe Text)
readFileSafe =
  liftIO . handleIf isDoesNotExistError (const $ pure Nothing) . fmap Just . T.readFile
