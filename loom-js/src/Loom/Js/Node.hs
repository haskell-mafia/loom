{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Js.Node (
    Node (..)
  , runNode
  , findNodeOnPath
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.List as L
import qualified Data.Text as T

import           Loom.Js

import           P

import           System.Exit (ExitCode (..))
import           System.IO (IO, FilePath)
import           System.Process (readProcessWithExitCode)

import           X.Control.Monad.Trans.Either


newtype Node = Node {
    nodeFilePath :: FilePath
  } deriving (Eq, Ord, Show)


runNode :: Node -> [[Char]] -> [Char] -> EitherT JsError IO Text
runNode (Node node) args stdin = do
  (ec, out, err) <- liftIO $ readProcessWithExitCode node args stdin
  case ec of
    ExitSuccess ->
      pure (T.pack out)
    ExitFailure x ->
      left (JsNodeExitFailure x (T.pack out) (T.pack err))

findNodeOnPath :: EitherT JsError IO Node
findNodeOnPath = do
  (ec, out, _err) <- liftIO $ readProcessWithExitCode "which" ["node"] []
  case (ec, L.lines out) of
    (ExitSuccess, node:[]) ->
      pure (Node node)
    _ ->
      left JsNodeMissing
