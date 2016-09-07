{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Loom.Cli.Javascript (
    BrowserifyInc (..)
  , BrowserifyArgs (..)
  , BrowserifyIncArgs (..)
  , BrowserifyRequire (..)
  , JavascriptManifest (..)
  , buildJavascript
  ) where

import qualified Data.Map as M
import qualified Data.Text as T

import           Loom.Cli.Build
import           Loom.Cli.File
import           Loom.Cli.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


newtype BrowserifyInc =
  BrowserifyInc {
      browserifyIncPath :: FilePath
    }

data JavascriptManifest =
  JavascriptManifest Text [FilePath]

data BrowserifyArgs =
  BrowserifyArgs {
      browserifyOutput :: FilePath
    -- Entry files
    , browserifyEntries :: [FilePath]
    -- an array of directories that browserify searches when looking for modules which are not referenced using relative path.
    , browserifyPaths :: [FilePath]
    -- Module file to bundle.require()
    , browserifyRequire :: [BrowserifyRequire]
    }

data BrowserifyIncArgs =
  BrowserifyIncArgs {
      browserifyIncCacheFile :: FilePath
    , browserifyIncArgs :: BrowserifyArgs
    }

data BrowserifyRequire =
  BrowserifyRequire {
      browserifyRequireExpose :: Text
    , browserifyRequirePath :: FilePath
    }

buildJavascript :: BrowserifyInc -> [BrowserifyRequire] -> EitherT ProcessError IO [JavascriptManifest]
buildJavascript bi requires = do
  -- FIX Configuration or convention?
  f <- buildBrowserify bi "first" $ \a -> a {
      browserifyEntries = ["app/js/src/first.js"]
    }
  compFiles <- findFiles ["modules/**/vanilla.js", "components/**/vanilla.js"] >>= \ps -> do
    -- Fix needs to be relative
    pure . flip fmap ps $ \p -> BrowserifyRequire (takeDirectory p) p
  m <- buildBrowserify bi "main" $ \a -> a {
      browserifyPaths = ["app/js/src", "modules", "components"]
    , browserifyRequire = requires <> compFiles
    }
  pure [f, m]

buildBrowserify :: BrowserifyInc -> Text -> (BrowserifyArgs -> BrowserifyArgs) -> EitherT ProcessError IO JavascriptManifest
buildBrowserify cmd n f =
  writeToFile ("tmp/js/" <> n <> ".js") $ \out -> do
    browserifyInc cmd $
      BrowserifyIncArgs
        ("tmp/js/browserify-cache-" <> n <> ".json")
        (f $ browserifyArgs out)
    -- FIX envify
    -- FIX if prod then uglify
    pure $ JavascriptManifest n [out]

browserifyArgs :: FilePath -> BrowserifyArgs
browserifyArgs out =
  BrowserifyArgs out [] [] []

browserifyInc :: BrowserifyInc -> BrowserifyIncArgs -> EitherT ProcessError IO ()
browserifyInc cmd (BrowserifyIncArgs cacheFile (BrowserifyArgs out entries paths require)) =
  -- FIX Need to use -c + envify + --detect-globals false to do dead code removal, especially in react code
  call' (M.singleton "NODE_PATH" . T.intercalate " " $ paths) (browserifyIncPath cmd) . mconcat $ [
      ["--cachefile", cacheFile]
    , ["-o", out]
    -- FIX Have a developer/production mode
    , ["--debug"]
    , entries
    -- NOTE _must_ start with "./" for relative files to work
    , require >>= \(BrowserifyRequire expose fp) -> ["-r", "./" <> fp <> ":" <> expose]
    ]
