import           Disorder.Core.Main

import qualified Test.Loom.Build.Data

main :: IO ()
main =
  disorderMain [
      Test.Loom.Build.Data.tests
    ]
