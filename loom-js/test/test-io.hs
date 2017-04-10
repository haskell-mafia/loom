import           Disorder.Core.Main

import qualified Test.IO.Loom.Js.Browserify as Browserify

main :: IO ()
main =
  disorderMain [
      Browserify.tests
    ]
