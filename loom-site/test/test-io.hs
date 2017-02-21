import           Disorder.Core.Main

import qualified Test.IO.Loom.Site

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Site.tests
    ]
