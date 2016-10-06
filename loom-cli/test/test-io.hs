import           Disorder.Core.Main

import qualified Test.IO.Loom.Cli.Env
import qualified Test.IO.Loom.Cli.Purescript

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Cli.Env.tests
    , Test.IO.Loom.Cli.Purescript.tests
    ]
