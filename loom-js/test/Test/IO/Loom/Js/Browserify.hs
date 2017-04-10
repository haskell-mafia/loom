{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Loom.Js.Browserify where


import qualified Data.Text as T

import           Disorder.Core
import           Disorder.Core.IO
import           Disorder.Either

import           Loom.Core.Data
import           Loom.Js
import           Loom.Js.Node
import           Loom.Js.Browserify

import           P

import           System.Directory
import           System.IO (IO)
import           System.IO.Temp

import           Test.QuickCheck (Property, (===), once)


prop_install :: Property
prop_install =
  once . withLoomHome $ \home ->
    testEitherT renderJsError $ do
      bfy1 <- installBrowserify home
      bfy2 <- installBrowserify home
      pure (bfy1 === bfy2)

prop_run_empty :: Property
prop_run_empty =
  once . withLoomHome $ \home ->
    testEitherT renderJsError $ do
      node <- findNodeOnPath
      brow <- installBrowserify home
      reso <- runBrowserify node brow BrowserifyInput {
          browserifyMode = BrowserifyProd
        , browserifyPaths = []
        , browserifyEntries = []
        }
      outp <- runNode node ["-e", T.unpack (unBrowserifyOutput reso)] []
      pure (outp === "")

withLoomHome :: (LoomHome -> IO Property) -> Property
withLoomHome f =
  testIO $ do
    tmpdir <- getTemporaryDirectory
    withTempDirectory tmpdir "loom" (f . LoomHome)

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
