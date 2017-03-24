{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Loom.Site where

import           Control.Monad.Trans.Class (lift)

import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as T

import           Loom.Build.Data
import           Loom.Core.Data
import           Loom.Site

import           Disorder.Core (ExpectedTestSpeed (..), disorderCheckEnvAll)
import           Disorder.Core.IO (testIO)
import           Disorder.Either (testEitherT)

import           P

import qualified System.Directory as Dir
import           System.FilePath ((</>), takeDirectory)
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import qualified Test.QuickCheck.Jack as J

import           X.Control.Monad.Trans.Either (newEitherT, runEitherT)

prop_site =
  J.once . testIO . testEitherT id . newEitherT . withTempDirectory "dist" "loom-site" $ \dir1 ->
    withTempDirectory "dist" "loom-site" $ \dirOut ->
      runEitherT $ do
        let r1 =
              LoomRoot dir1
            f1 =
              LoomFile r1 "f1"
            components =
              Map.fromList
                [ ( LoomName "test"
                  , [ Component
                        (LoomFile r1 "c1")
                        [ComponentFile f1 "x.scss"]
                        [ComponentFile f1 "x.prj"]
                        [ComponentFile f1 "x.mcn"]
                        [ComponentFile f1 "x.svg"]
                    ])
                ]
            assertFileExists f =
              fmap (J.counterexample f) . Dir.doesFileExist $ f
            writeFile f t =
              Dir.createDirectoryIfMissing True (takeDirectory f) >> T.writeFile f t
        lift $ writeFile (dir1 </> "test.css") ""
        lift $ writeFile (dir1 </> "f1/x.svg") ""
        firstT renderLoomSiteError $
          generateLoomSite
            (LoomSitePrefix "/")
            (LoomSiteRoot dirOut)
            (AssetsPrefix "assets")
            (LoomResult
               (LoomName "test")
               components
               mempty
               mempty
               (CssFile $ dir1 </> "test.css")
               [ImageFile (LoomName "c1") (ComponentFile f1 "x.svg")])
        fmap J.conjoin . lift . sequence $
          [ assertFileExists $ dirOut </> "index.html"
          , assertFileExists $ dirOut </> "components" </> "index.html"
          , assertFileExists $ dirOut </> "components" </> "test" </> "c1" </> "index.html"
          , assertFileExists $ dirOut </> "static" </> "loom.css"
          , assertFileExists $ dirOut </> "assets" </> "test.css"
          , assertFileExists $ dirOut </> "assets" </> "c1" </> "f1" </> "x.svg"
          ]

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunFewer
