module Bar (bar) where

foreign import third :: String

bar :: String
bar =
  third
