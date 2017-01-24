import           Disorder.Core.Main

import qualified Test.IO.Loom.Purescript

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Purescript.tests
    ]
