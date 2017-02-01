import           Disorder.Core.Main

import qualified Test.IO.Loom.Build.Component
import qualified Test.IO.Loom.Build.Data

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Build.Component.tests
    , Test.IO.Loom.Build.Data.tests
    ]
