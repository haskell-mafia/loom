module Test.Bar (main) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Data.Unit (Unit)
import Bar as Bar

main :: forall eff. Eff (console :: Console.CONSOLE | eff) Unit
main =
  Console.log Bar.bar
