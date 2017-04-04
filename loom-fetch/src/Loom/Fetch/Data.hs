{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Fetch.Data (
    Fetcher (..)
  ) where


import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


data Fetcher a e b = Fetcher {
    runFetcher :: a -> EitherT e IO b
  }
