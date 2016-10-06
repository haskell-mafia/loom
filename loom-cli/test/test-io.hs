import           Disorder.Core.Main

import qualified Test.IO.Loom.Cli.Env

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Cli.Env.tests
    ]
