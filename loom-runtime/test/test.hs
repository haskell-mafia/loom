import           Disorder.Core.Main

import qualified Test.Loom.Runtime.Wai

main :: IO ()
main =
  disorderMain [
      Test.Loom.Runtime.Wai.tests
    ]
