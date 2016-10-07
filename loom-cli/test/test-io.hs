import           Disorder.Core.Main

import qualified Test.IO.Loom.Cli.Env
import qualified Test.IO.Loom.Cli.Purescript
import qualified Test.IO.Loom.Cli.Sass

main :: IO ()
main =
  disorderMain [
      Test.IO.Loom.Cli.Env.tests
    , Test.IO.Loom.Cli.Purescript.tests
    , Test.IO.Loom.Cli.Sass.tests
    ]
