{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_loom_cli

import           DependencyInfo_ambiata_loom_cli

import           Loom.Cli

import           P

import           System.Exit (exitSuccess)
import           System.IO (BufferMode (..), IO, hSetBuffering, stderr, stdout, print, putStrLn)

import           X.Options.Applicative (RunType (..), Parser (..), SafeCommand (..))
import qualified X.Options.Applicative as OA

import           X.Control.Monad.Trans.Either.Exit (orDie)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  OA.dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      DependencyCommand ->
        mapM_ putStrLn dependencyInfo
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

parser :: Parser (SafeCommand Command)
parser =
  OA.safeCommand $ pure Command

run :: Command -> IO ()
run c = case c of
  Command ->
    orDie renderLoomError loom

data Command =
  Command
  deriving (Eq, Show)
