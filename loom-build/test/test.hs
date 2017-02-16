import           Disorder.Core.Main

import qualified Test.Loom.Build.Assets
import qualified Test.Loom.Build.Data

main :: IO ()
main =
  disorderMain [
      Test.Loom.Build.Assets.tests
    , Test.Loom.Build.Data.tests
    ]
