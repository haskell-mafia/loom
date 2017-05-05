{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Purescript (
    LoomPurescriptError (..)
  , generatePurescript
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Loom.Core.Data
import           Loom.Machinator (MachinatorPurescriptError, MachinatorOutput)
import qualified Loom.Machinator as Machinator
import           Loom.Projector (ProjectorPurescriptError)
import qualified Loom.Projector as Projector
import           Loom.Purescript

import           P

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), takeFileName)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data LoomPurescriptError =
    LoomPurescriptProjectorError ProjectorPurescriptError
  | LoomPurescriptMachinatorError MachinatorPurescriptError
  deriving (Show)

generatePurescript ::
     PurescriptUnpackDir
  -> CssFile
  -> [ImageFile]
  -> [(BundleName, JsFile)]
  -> MachinatorOutput
  -> Projector.ProjectorOutput
  -> EitherT LoomPurescriptError IO ()
-- FIXME real type signature should be like so
-- LoomSitePrefix -> AssetsPrefix -> LoomResult -> EitherT LoomPurescriptError IO ()
generatePurescript (PurescriptUnpackDir output) inputCss images inputJs mo po = do
  void . firstT LoomPurescriptMachinatorError $
    Machinator.generateMachinatorPurescript
      (output </> "loom-data" </> "src")
      (Machinator.ModuleName . Projector.unModuleName <$> Projector.requiredProjectorPurescriptImports)
      mo
{-  let
    --n = (T.map (\c -> if Char.isAlphaNum c then Char.toLower c else '-') . renderLoomName) name <> "loom"
    n = "loom-projector"
    outputCss = CssFile $ output </> (takeFileName . renderCssFile) inputCss
    outputJs' = with inputJs . fmap $ \jsfile -> JsFile $ output </> takeFileName (renderJsFile jsfile)
    output' = output </> T.unpack n </> "src"
    spx = LoomSitePrefix "FIXME"
    apx = AssetsPrefix "FIXME"
  liftIO $
    createDirectoryIfMissing True output
  void . firstT LoomPurescriptProjectorError $
    Projector.generateProjectorPurescript output' spx apx [outputCss] images outputJs' po -}
