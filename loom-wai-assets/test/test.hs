import           Disorder.Core.Main

import qualified Test.Loom.Wai.Assets

main :: IO ()
main =
  disorderMain [
      Test.Loom.Wai.Assets.tests
    ]
