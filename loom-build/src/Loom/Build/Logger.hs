{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Loom.Build.Logger (
    Logger (..)
  , hoistLogger
  , withLog
  , withLogIO
  ---
  , newSimpleLogger
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Time as Time
import qualified Data.Text as T

import           P

import           System.IO (IO)
import qualified System.IO as IO

-- | Data type that allows the start/end of a given computation to be logged.
--
-- NOTE: The obvious `forall a. Text -> m a -> m a` representation doesn't allow
-- hoistLogger, which makes it unusable.
--
data Logger m =
  forall s.
  Logger {
      withLogBefore :: Text -> m s
    , withLogAfter :: Text -> s -> m ()
    }

hoistLogger :: (forall a. m a -> n a) -> Logger m -> Logger n
hoistLogger f (Logger b a) =
  Logger
    (f . b)
    (\t -> f . a t)

withLog :: Monad m => Logger m -> Text -> m a -> m a
withLog (Logger before after) t m = do
  b <- before t
  a <- m
  after t b
  pure a

withLogIO :: MonadIO m => Logger IO -> Text -> m a -> m a
withLogIO l =
  withLog (hoistLogger liftIO l)

--------

-- | A simple logger that logs the start of a build, and the time taken when it finishes.
--
-- NOTE: Won't work very well with concurrent output!
--
newSimpleLogger :: MonadIO m => IO.Handle -> Logger m
newSimpleLogger h =
  Logger
    (\n -> do
      t1 <- liftIO Time.getCurrentTime
      liftIO $
        IO.hPutStr h $ "Building " <> T.unpack n <> "..."
      liftIO $
        IO.hFlush h
      pure t1
     )
    (\_ t1 -> do
      t2 <- liftIO Time.getCurrentTime
      liftIO $
        IO.hPutStrLn h $ " [" <> (show . Time.diffUTCTime t2) t1 <> "]"
      )
