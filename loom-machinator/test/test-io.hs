import           Disorder.Core.Main

import qualified Test.IO.Loom.Machinator

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Machinator.tests
    ]
