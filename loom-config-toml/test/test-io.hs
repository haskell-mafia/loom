import           Disorder.Core.Main

import qualified Test.IO.Loom.Config.Toml

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Config.Toml.tests
    ]
