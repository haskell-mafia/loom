{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Purescript (
    LoomPurescriptError (..)
  , generatePurescript
  , renderLoomPurescriptError
  ) where


import           Loom.Build.Data
import           Loom.Core.Data
import           Loom.Projector (ProjectorPurescriptError)
import qualified Loom.Projector as Projector
import qualified Loom.Machinator as Machinator

import           P

import           System.FilePath ((</>), FilePath, takeFileName)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data LoomPurescriptError =
    LoomPurescriptMachinatorError Machinator.MachinatorPurescriptError
  | LoomPurescriptProjectorError ProjectorPurescriptError

renderLoomPurescriptError :: LoomPurescriptError -> Text
renderLoomPurescriptError he =
  case he of
    LoomPurescriptMachinatorError e ->
      Machinator.renderMachinatorPurescriptError e
    LoomPurescriptProjectorError e ->
      Projector.renderProjectorPurescriptError e


generatePurescript ::
     FilePath
  -> LoomSitePrefix
  -> AssetsPrefix
  -> LoomResult
  -> EitherT LoomPurescriptError IO ()
generatePurescript output spx apx (LoomResult _name _ mo po inputCss images inputJs) = do
  void . firstT LoomPurescriptMachinatorError $
    Machinator.generateMachinatorPurescript
      (output </> "src")
      (Machinator.ModuleName . Projector.unModuleName <$> Projector.requiredProjectorPurescriptImports)
      mo
  let
    outputCss = CssFile $ cssFileOutput inputCss
    outputJs' = fmap (fmap (JsFile . jsFileOutput)) inputJs
  void . firstT LoomPurescriptProjectorError $
    Projector.generateProjectorPurescript (output </> "src") spx apx [outputCss] images outputJs' po


cssFileOutput :: CssFile -> FilePath
cssFileOutput =
  takeFileName . renderCssFile

jsFileOutput :: JsFile -> FilePath
jsFileOutput =
  takeFileName . renderJsFile
