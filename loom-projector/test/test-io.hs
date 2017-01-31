import           Disorder.Core.Main

import qualified Test.IO.Loom.Projector

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Projector.tests
    ]
