{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_loom_cli

import           DependencyInfo_ambiata_loom_cli

import           Loom.Build.Core

import           P

import           System.Directory (getCurrentDirectory)
import           System.IO (BufferMode (..), IO, hSetBuffering, stderr, stdout)

import           X.Options.Applicative (Parser (..))
import qualified X.Options.Applicative as OA

import           X.Control.Monad.Trans.Either.Exit (orDie)

data Command =
    Build
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  OA.cli "loom" buildInfoVersion dependencyInfo parser $ \c ->
    case c of
      Build -> do
        cwd <- getCurrentDirectory
        orDie renderLoomError $ buildLoom cwd

parser :: Parser Command
parser =
  OA.subparser . mconcat $ [
      OA.command' "build" "Build a loom project from the current working directory" $
        pure Build
    ]

renderLoomError :: LoomError -> Text
renderLoomError le =
  case le of
    LoomError ->
      "The impossible happened!"
