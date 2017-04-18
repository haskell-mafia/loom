module Loom.Test.Purs.C2 (
    foo
  , bar
  , foobar
  ) where

import Prelude ((<>))

foo :: String
foo =
  "foo"

bar :: String
bar =
  "bar"

foobar :: String
foobar =
  foo <> bar
