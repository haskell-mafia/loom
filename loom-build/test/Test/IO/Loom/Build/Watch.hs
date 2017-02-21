{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Build.Watch where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async

import qualified Data.Text.IO as T
import qualified Data.IORef as IORef

import           Loom.Build.Watch

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)

import           P

import           System.FilePath ((</>))
import           System.IO (IO, putStrLn)
import           System.IO.Temp (withTempDirectory)

import           Test.QuickCheck.Jack ((===), once)

import qualified Twine.Data.Pin as Pin

prop_build_watch_last_wins =
  once . testIO . withTempDirectory "dist" "loom-watch" $ \dir -> do
    pin <- Pin.newPin
    var1 <- IORef.newIORef []
    var2 <- IORef.newIORef []
    p2 <- Pin.newPin
    p3 <- Pin.newPin
    p4 <- Pin.newPin
    a1 <- Async.async $ do
      putStrLn "Wait for p2"
      Pin.pullPin p2
      watchTreeWithCancel pin dir (/= ".") $ \f -> do
        IORef.modifyIORef var1 (f :)
        if f == "a"
          then do
            putStrLn "pull p3"
            Pin.pullPin p3
            putStrLn "Wait p4"
            Pin.waitForPin p4
            -- Never gets to this line
            IORef.modifyIORef var2 (f :)
          else do
            putStrLn "Pull p4"
            Pin.pullPin p4
            IORef.modifyIORef var2 (f :)
            putStrLn "Pull pin"
            Pin.pullPin pin
    putStrLn "Wait p2"
    Pin.waitForPin p2
    -- FIX This is lazy, but otherwise we sometimes hang due to a delay in the system watching
    threadDelay 10000
    T.writeFile (dir </> "a") ""
    putStrLn "Wait p3"
    Pin.waitForPin p3
    T.writeFile (dir </> "b") ""
    putStrLn "Wait a1"
    Async.wait a1
    f1 <- IORef.readIORef var1
    f2 <- IORef.readIORef var2
    -- Make sure the first file watched was cancelled as expected
    pure $ (f1, f2) === (["b", "a"], ["b"])

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
