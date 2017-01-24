import           Disorder.Core.Main

import qualified Test.IO.Loom.Sass

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Sass.tests
    ]
