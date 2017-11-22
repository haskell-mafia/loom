module Libby where

import Data.Semigroup ((<>))

hello :: String -> String
hello person = "Hello, " <> person
