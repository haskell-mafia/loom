{-# LANGUAGE NoImplicitPrelude #-}
module Loom.Build (
    LoomError (..)
  , buildLoom
  ) where

import           P

import           System.FilePath (FilePath)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

data LoomError =
    LoomError
  deriving (Eq, Show)

buildLoom :: FilePath -> EitherT LoomError IO ()
buildLoom _ =
  pure ()
