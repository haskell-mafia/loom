{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Projector (
    ProjectorError (..)
  , ProjectorHaskellError (..)
  , ProjectorInterpretError (..)
  , ProjectorInput (..)
  , ProjectorOutput
  , MachinatorModules
  , projectorOutputModules
  , projectorOutputModuleExprs
  , DataModuleName (..)
  , ModuleName (..)
  , Projector.Html (..)
  , Projector.Attribute (..)
  , compileProjector
  , generateProjectorHtml
  , generateProjectorHaskell
  , moduleNameFromFile
  , requiredProjectorHaskellImports
  , renderProjectorError
  , renderProjectorHaskellError
  , renderProjectorInterpretError
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Catch (handleIf)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Machinator.Core as MC

import           P

import           Projector.Html (BuildArtefacts (..), DataModuleName (..), ModuleName (..), HtmlExpr, HtmlType)
import           Projector.Html.Data.Annotation (SrcAnnotation)
import qualified Projector.Html as Projector
import qualified Projector.Html.Backend.Haskell as Projector
import qualified Projector.Html.Core.Machinator as Projector
import qualified Projector.Html.Data.Module as Projector
import qualified Projector.Html.Interpreter as Projector
import qualified Projector.Core as Projector

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (FilePath, (</>), takeDirectory, takeFileName, dropExtension, makeRelative)
import           System.IO (IO)
import           System.IO.Error (isDoesNotExistError)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT, hoistEither)

data ProjectorError =
    ProjectorFileMissing FilePath
  | ProjectorError [Projector.HtmlError]
    deriving (Eq, Show)

data ProjectorHaskellError =
    ProjectorHaskellError [Projector.HaskellError]
  deriving (Eq, Show)

data ProjectorInterpretError =
    ProjectorInterpretError (Projector.InterpretError (HtmlType, SrcAnnotation))
  deriving (Eq, Show)

data ProjectorInput =
  ProjectorInput {
      projectorLoomName :: Text
    , projectorModuleRoot :: FilePath
    , projectorModuleTemplates :: [FilePath]
    } deriving (Show)

data ProjectorOutput =
  ProjectorOutput {
      _projectorOutputArtefacts :: BuildArtefacts
    }

instance Monoid ProjectorOutput where
  mempty =
    ProjectorOutput (BuildArtefacts mempty mempty)
  mappend (ProjectorOutput (BuildArtefacts d1 d3)) (ProjectorOutput (BuildArtefacts d2 d4)) =
    ProjectorOutput (BuildArtefacts (d1 <> d2) (d3 <> d4))

projectorOutputModules :: ProjectorOutput -> [ModuleName]
projectorOutputModules (ProjectorOutput (BuildArtefacts _nmap ms)) =
  Map.keys ms

projectorOutputModuleExprs ::
  ProjectorOutput ->
  Map ModuleName [HtmlExpr (HtmlType, SrcAnnotation)]
projectorOutputModuleExprs (ProjectorOutput (BuildArtefacts _nmap ms)) =
  fmap (fmap snd . Map.elems . Projector.moduleExprs) $ ms

-- FIX Should be in machinator
type MachinatorModules = Map.Map Projector.DataModuleName [MC.Definition]

compileProjector ::
  MachinatorModules ->
  ProjectorOutput ->
  ProjectorInput ->
  EitherT ProjectorError IO ProjectorOutput
compileProjector
  udts
  (ProjectorOutput (BuildArtefacts nmap1 oh1))
  (ProjectorInput prefix root inputs) = do

  templates <- for inputs $ \input ->
    fmap ((,) input) .
      newEitherT . fmap (maybeToRight (ProjectorFileMissing input)) . readFileSafe $
        input
  let
    decls =
      Projector.machinatorDecls . join . Map.elems $ udts
  BuildArtefacts nmap2 oh2 <- firstT ProjectorError . hoistEither $
    Projector.runBuildIncremental
      (Projector.Build (moduleNamer prefix root) (Map.keys udts))
      (Projector.UserDataTypes decls)
      oh1
      (Projector.RawTemplates templates)
  hoistEither . first ProjectorError $
    Projector.warnModules decls oh2
  pure $ ProjectorOutput (BuildArtefacts (nmap1 <> nmap2) oh2)

generateProjectorHtml ::
  MachinatorModules ->
  ProjectorOutput ->
  Projector.HtmlExpr (HtmlType, SrcAnnotation) ->
  Either ProjectorInterpretError Projector.Html
generateProjectorHtml mo (ProjectorOutput (BuildArtefacts _ h)) =
  first ProjectorInterpretError . Projector.interpret
    (Projector.machinatorDecls . join $ Map.elems mo)
    (Projector.extractModuleExprs h)

generateProjectorHaskell :: FilePath -> ProjectorOutput -> EitherT ProjectorHaskellError IO [FilePath]
generateProjectorHaskell output (ProjectorOutput ba) = do
  fs <- hoistEither . first ProjectorHaskellError $
    Projector.codeGen Projector.haskellBackend codeGenNamer ba
  for fs $ \(f, t) -> do
    liftIO $
      createDirectoryIfMissing True (output </> takeDirectory f)
    liftIO $
      T.writeFile (output </> f) t
    pure f

moduleNamer :: Text -> FilePath -> Projector.ModuleNamer
moduleNamer prefix root =
  let mnr = Projector.moduleNamerSimple (Just (moduleNameFromFile (T.unpack prefix)))
  in Projector.ModuleNamer
      (Projector.pathToModuleName mnr . takeDirectory . makeRelative root)
      (\fp -> Projector.Name . T.pack $ T.unpack prefix </> (dropExtension . makeRelative root $ fp))

codeGenNamer :: Projector.CodeGenNamer
codeGenNamer =
  Projector.CodeGenNamer
    -- Names for invocations
    (\_n (Projector.ModuleName mn) fp -> Projector.Name (mn <> "." <> file fp))
    -- Names for definitions
    (\_n _mn fp -> Projector.Name (file fp))
  where
    file = T.pack . dropExtension . takeFileName

moduleNameFromFile :: FilePath -> ModuleName
moduleNameFromFile =
  Projector.pathToModuleName (Projector.moduleNamerSimple Nothing)

requiredProjectorHaskellImports :: [ModuleName]
requiredProjectorHaskellImports =
  [
    Projector.ModuleName "Projector.Html.Runtime"
  ]

renderProjectorError :: ProjectorError -> Text
renderProjectorError pe =
  case pe of
    ProjectorFileMissing f ->
      "Could not find file: " <> T.pack f
    ProjectorError es ->
      "Projector build errors:\n" <> T.unlines (fmap Projector.renderHtmlError es)

renderProjectorHaskellError :: ProjectorHaskellError -> Text
renderProjectorHaskellError pe =
  case pe of
    ProjectorHaskellError es ->
      "Projector haskell errors:\n" <> (T.unlines . fmap Projector.renderHaskellError) es

renderProjectorInterpretError :: ProjectorInterpretError -> Text
renderProjectorInterpretError pe =
  case pe of
    ProjectorInterpretError (Projector.InterpretInvalidExpression e) ->
      -- FIX Better error message from projector
      "Error interpretting expression:\n" <> Projector.ppExpr e

-- FIX Common module?
readFileSafe :: MonadIO m => FilePath -> m (Maybe Text)
readFileSafe =
  liftIO . handleIf isDoesNotExistError (const $ pure Nothing) . fmap Just . T.readFile
