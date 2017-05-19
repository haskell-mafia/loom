{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Js.Node (
    Node (..)
  , NodePath (..)
  , runNode
  , runNodeMain
  , findNodeOnPath
  ) where


import           Control.Monad.IO.Class (liftIO)

import qualified Data.List as L
import qualified Data.Text as T

import           Loom.Js

import           P

import qualified System.Environment as Environment
import           System.Exit (ExitCode (..))
import           System.IO (IO, FilePath)
import           System.Process (readProcessWithExitCode)
import qualified System.Process as Process

import           X.Control.Monad.Trans.Either


newtype Node = Node {
    nodeFilePath :: FilePath
  } deriving (Eq, Ord, Show)

newtype NodePath =
  NodePath {
      unNodePath :: [FilePath]
    } deriving (Eq, Show)

runNode :: Node -> [[Char]] -> [Char] -> EitherT JsError IO Text
runNode (Node node) args stdin = do
  (ec, out, err) <- liftIO $ readProcessWithExitCode node args stdin
  case ec of
    ExitSuccess ->
      pure (T.pack out)
    ExitFailure x ->
      left (JsNodeExitFailure x (T.pack out) (T.pack err))

-- | Run a main function, inheriting stdout/stderr
runNodeMain :: Node -> NodePath -> JsModuleName -> IO ExitCode
runNodeMain (Node node) (NodePath paths) main = do
  env <- Environment.getEnvironment
  (_stdin, _stdout, _stderr, pid) <- Process.createProcess
    (Process.proc
      node
      ["-e", "require(\"" <> (T.unpack . unJsModuleName) main <> "\").main()"]
      ) {
        Process.env = Just $ env <> [("NODE_PATH", intercalate ":" paths)]
      }
  Process.waitForProcess pid

findNodeOnPath :: EitherT JsError IO Node
findNodeOnPath = do
  (ec, out, _err) <- liftIO $ readProcessWithExitCode "which" ["node"] []
  case (ec, L.lines out) of
    (ExitSuccess, node:[]) ->
      pure (Node node)
    _ ->
      left JsNodeMissing
