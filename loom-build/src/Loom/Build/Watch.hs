{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Build.Watch (
    loomWatch
  , loomWatchPredicate
  ---
  , watchTreeWithCancel
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar

import           Loom.Core.Data

import           P

import qualified System.Directory as Dir
import           System.FilePath (FilePath, makeRelative)
import           System.IO (IO)

import qualified System.FSNotify as FSNotify

import           Twine.Data.Pin (Pin)
import qualified Twine.Data.Pin as Pin

-- | Watch any of the current loom paths, running a given action.
-- If a file is modified during the build the previous invocation will be cancelled and re-run.
loomWatch :: Pin -> FilePath -> Loom -> (FilePath -> IO ()) -> IO ()
loomWatch pin root loom =
  watchTreeWithCancel pin root (loomWatchPredicate loom)

-- | A specific predicate based on the current loom configuration
loomWatchPredicate :: Loom -> FilePath -> Bool
loomWatchPredicate loom =
  matchFilePatterns (loomWatchPatterns loom)

-- | Watch a given file tree, cancelling the current action if another file is touched
watchTreeWithCancel :: Pin -> FilePath -> (FilePath -> Bool) -> (FilePath -> IO ()) -> IO ()
watchTreeWithCancel pin root pred run =
  FSNotify.withManager $ \wm -> do
    v <- MVar.newMVar Nothing
    -- FSNotify returns paths that are canonicalized regardless of the root provided
    root' <- Dir.canonicalizePath root
    sl <- FSNotify.watchTree wm root (pred . makeRelative root' . FSNotify.eventPath) $ \e -> do
      a <- MVar.modifyMVar v $ \b -> do
        -- Cancel the previous build before we start
        for_ b $ Async.cancel
        a <- Async.async . run . makeRelative root' . FSNotify.eventPath $ e
        pure (Just a, a)
      -- Match sure we catch here to ensure the watcher keeps running
      void $ Async.waitCatch a
    Pin.waitForPin pin
    MVar.takeMVar v >>= mapM_ Async.wait
    sl
