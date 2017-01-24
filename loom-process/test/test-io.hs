import           Disorder.Core.Main

import qualified Test.IO.Loom.Process

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Process.tests
    ]
