{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Loom.Projector (
    ProjectorError (..)
  , ProjectorHaskellError (..)
  , ProjectorInterpretError (..)
  , ProjectorInput (..)
  , ProjectorOutput
  , ProjectorTerm (..)
  , projectorTermMap
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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Loom.Core.Data

import qualified Machinator.Core as MC

import           P

import           Projector.Html (BuildArtefacts (..), DataModuleName (..), ModuleName (..), HtmlExpr, HtmlType)
import           Projector.Html.Data.Annotation (SrcAnnotation)
import qualified Projector.Html as Projector
import qualified Projector.Html.Backend.Haskell as Projector
import qualified Projector.Html.Core.Machinator as Projector
import qualified Projector.Html.Data.Module as Projector
import qualified Projector.Html.Data.Prim as Projector
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
    , projectorLoomImageFile :: [ImageFile]
    , projectorLoomJs :: [(BundleName, JsFile)]
    , projectorDataTypes :: MachinatorModules
    , projectorModuleTemplates :: [FilePath]
    } deriving (Show)

data ProjectorOutput =
  ProjectorOutput {
      _projectorOutputArtefacts :: BuildArtefacts
    , projectorOutputUsefulMap :: Map FilePath ProjectorTerm
    }

projectorTermMap :: ProjectorOutput -> Map FilePath ProjectorTerm
projectorTermMap =
  projectorOutputUsefulMap

data ProjectorTerm = ProjectorTerm {
    projectorTermName :: Projector.Name
  , projectorTermType :: Projector.HtmlType
  , projectorTermExpr :: Projector.HtmlExpr (HtmlType, SrcAnnotation)
  } deriving (Eq, Ord, Show)

instance Monoid ProjectorOutput where
  mempty =
    ProjectorOutput (BuildArtefacts mempty mempty mempty) mempty
  mappend (ProjectorOutput (BuildArtefacts d1 d3 d5) u1) (ProjectorOutput (BuildArtefacts d2 d4 d6) u2) =
    ProjectorOutput (BuildArtefacts (d1 <> d2) (d3 <> d4) (d5 <> d6)) (u1 <> u2)

projectorOutputModules :: ProjectorOutput -> [ModuleName]
projectorOutputModules (ProjectorOutput (BuildArtefacts _decls _nmap ms) _um) =
  Map.keys ms

projectorOutputModuleExprs ::
  ProjectorOutput ->
  Map ModuleName [HtmlExpr (HtmlType, SrcAnnotation)]
projectorOutputModuleExprs (ProjectorOutput (BuildArtefacts _decls _nmap ms) _um) =
  fmap (fmap Projector.meExpr . Map.elems . Projector.moduleExprs) $ ms

-- FIX Should be in machinator
type MachinatorModules = Map.Map FilePath [MC.Definition]

compileProjector ::
  ProjectorOutput ->
  ProjectorInput ->
  EitherT ProjectorError IO ProjectorOutput
compileProjector
  (ProjectorOutput (BuildArtefacts decls1 nmap1 oh1) um)
  (ProjectorInput prefix root images js mo inputs) = do
  templates <- for inputs $ \input ->
    fmap ((,) input) .
      newEitherT . fmap (maybeToRight (ProjectorFileMissing input)) . readFileSafe $
        input
  let
    udts' = with mo Projector.machinatorDecls
    decls = decls1 <> fold (Map.elems udts')
  BuildArtefacts decls2 nmap2 oh2 <- firstT ProjectorError . hoistEither $
    Projector.runBuildIncremental
      (Projector.Build (moduleNamer prefix root) mempty)
      (Projector.UserDataTypes (fmap (second Projector.unTypeDecls) (Map.toList udts')))
      (userConstants images js)
      decls
      oh1
      (Projector.RawTemplates templates)
  hoistEither . first ProjectorError $
    Projector.warnModules decls oh2
  let
    bas = BuildArtefacts (decls <> decls2) (nmap1 <> nmap2) oh2
    uma = um <> makeUsefulMap bas
  pure $ ProjectorOutput bas uma

generateProjectorHtml ::
     MachinatorModules
  -> LoomSitePrefix
  -> AssetsPrefix
  -> [CssFile]
  -> [ImageFile]
  -> [(BundleName, JsFile)]
  -> ProjectorOutput
  -> Projector.HtmlExpr (HtmlType, SrcAnnotation)
  -> Either ProjectorInterpretError Projector.Html
generateProjectorHtml mo spfx apfx css images js (ProjectorOutput (BuildArtefacts _ _ h) _um) =
  first ProjectorInterpretError . Projector.interpret
    (Projector.machinatorDecls . join $ Map.elems mo)
    (Projector.extractModuleExprs h <> Projector.platformConstants (platformConstants spfx apfx css images js))

generateProjectorHaskell ::
     FilePath
  -> LoomSitePrefix
  -> AssetsPrefix
  -> [CssFile]
  -> [ImageFile]
  -> [(BundleName, JsFile)]
  -> ProjectorOutput
  -> EitherT ProjectorHaskellError IO [FilePath]
generateProjectorHaskell output spfx apfx css images js (ProjectorOutput ba _um) = do
  fs <- hoistEither . first ProjectorHaskellError $
    Projector.codeGen Projector.haskellBackend codeGenNamer (platformConstants spfx apfx css images js) ba
  for fs $ \(f, t) -> do
    liftIO $
      createDirectoryIfMissing True (output </> takeDirectory f)
    liftIO $
      T.writeFile (output </> f) t
    pure f

moduleNamer :: Text -> FilePath -> Projector.ModuleNamer
moduleNamer prefix root =
  let mnr =
        Projector.moduleNamerSimple
          (Just (moduleNameFromFile (T.unpack prefix)))
      dropFileIfDefault fp =
        if takeFileName (dropExtension fp) == defaultTemplateName
          then takeDirectory fp
          else fp
  in Projector.ModuleNamer
       (Projector.pathToModuleName mnr . takeDirectory . makeRelative root)
       (Projector.pathToDataModuleName mnr . takeDirectory . makeRelative root)
       (\fp -> Projector.Name . T.pack $
         T.unpack prefix </> (dropFileIfDefault . dropExtension . makeRelative root $ fp))

defaultTemplateName :: [Char]
defaultTemplateName =
  "template"

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

makeUsefulMap :: BuildArtefacts -> Map FilePath ProjectorTerm
makeUsefulMap (BuildArtefacts _decls (Projector.TemplateNameMap nmap) mmap) =
  Map.fromList . catMaybes . with (Map.toList nmap) $ \(name, (modn, file)) -> do
    modl <- Map.lookup modn mmap
    Projector.ModuleExpr ty expr <- Map.lookup name (Projector.moduleExprs modl)
    pure (file, ProjectorTerm name ty expr)

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

userConstants :: [ImageFile] -> [(BundleName, JsFile)] -> Projector.UserConstants
userConstants images js =
  Projector.UserConstants . Map.fromList $
       (cssName, cssType)
    :  (jsName, jsType)
    :  fmap ((, imageType) . imageName) images
    <> fmap (bimap jsIndividualName (const stringT)) js

platformConstants ::
     LoomSitePrefix
  -> AssetsPrefix
  -> [CssFile]
  -> [ImageFile]
  -> [(BundleName, JsFile)]
  -> Projector.PlatformConstants
platformConstants spfx apfx css images js =
  Projector.PlatformConstants . Map.fromList $
       (cssName, mkStringList (fmap (cssAssetPath spfx apfx) css))
    :  (jsName, mkStringList (fmap (jsAssetPath spfx apfx . snd) js))
    :  fmap (\i -> (imageName i, mkString (imageAssetPath spfx apfx i))) images
    <> fmap (\(b, f) -> (jsIndividualName b, mkString (jsAssetPath spfx apfx f))) js

cssName :: Projector.Name
cssName =
  Projector.Name "loom/css"

cssType :: Projector.HtmlType
cssType =
  Projector.TList stringT

jsName :: Projector.Name
jsName =
  Projector.Name "loom/js"

jsType :: Projector.HtmlType
jsType =
  Projector.TList stringT

jsIndividualName :: BundleName -> Projector.Name
jsIndividualName (BundleName b) =
  Projector.Name ("loom/js/" <> b)

imageType :: Projector.HtmlType
imageType =
  stringT

imageName :: ImageFile -> Projector.Name
imageName =
  Projector.Name . T.replace "." "/" . T.pack . imageFilePathNoRoot

mkString :: Text -> Projector.HtmlExpr Projector.HtmlType
mkString =
  Projector.ELit stringT . Projector.VString

mkStringList :: [Text] -> Projector.HtmlExpr Projector.HtmlType
mkStringList ts =
  Projector.EList stringListT (fmap mkString ts)

stringT :: Projector.HtmlType
stringT =
  Projector.TLit Projector.TString

stringListT :: Projector.HtmlType
stringListT =
  Projector.TList stringT
