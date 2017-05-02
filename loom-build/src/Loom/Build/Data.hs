module Loom.Build.Data (
    LoomResult (..)
  ) where

import           Data.Map.Strict (Map)

import           Loom.Core.Data
import           Loom.Machinator (MachinatorOutput)
import           Loom.Projector (ProjectorOutput)

data LoomResult =
  LoomResult {
      loomResultName :: LoomName
    , loomResultComponents :: Map LoomName [Component]
    , loomResultMachinatorOutput :: MachinatorOutput
    , loomResultProjectorOutput :: ProjectorOutput
    , loomResultCss :: CssFile
    , loomResultImages :: [ImageFile]
    , loomResultJs :: [(BundleName, JsFile)]
    }
