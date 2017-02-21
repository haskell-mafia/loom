{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Loom.Site (
    LoomSiteRoot (..)
  , LoomSiteError (..)
  , SiteNavigation (..)
  , SiteTitle (..)
  , defaultLoomSiteRoot
  , generateLoomSite
  , cleanLoomSite
  , generateLoomSiteStatic
  , renderHtmlErrorPage
  , loomSiteError
  , loomSiteNotFound
  , renderLoomSiteError
  ) where

import           Control.Monad.Catch (handleIf)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.FileEmbed (embedFile)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import           Loom.Build.Data
import           Loom.Sass (CssFile (..), renderCssFile)

import           P

import qualified System.Directory as Dir
import           System.FilePath (FilePath, (</>))
import qualified System.FilePath as File
import           System.IO (IO)
import qualified System.IO.Error as IO.Error
import           System.IO.Error (IOError, tryIOError)

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Html.Renderer.Text (renderHtml)

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)

newtype LoomSiteRoot =
  LoomSiteRoot {
      loomSiteRootFilePath :: FilePath
    } deriving (Eq, Show)

data SiteNavigation =
    SiteHome
  | SiteComponents
  deriving (Bounded, Eq, Enum, Show)

newtype SiteTitle =
  SiteTitle {
      renderSiteTitle :: Text
    } deriving (Eq, Show)

data LoomSiteError =
    LoomSiteFileError IOError
  deriving (Eq, Show)

data HtmlFile =
  HtmlFile FilePath SiteNavigation SiteTitle Html

defaultLoomSiteRoot :: Loom -> LoomSiteRoot
defaultLoomSiteRoot c =
  LoomSiteRoot $
    loomOutput c </> "site"

generateLoomSite :: LoomSitePrefix -> LoomSiteRoot -> AssetsPrefix -> LoomResult -> EitherT LoomSiteError IO ()
generateLoomSite prefix (LoomSiteRoot out) apx (LoomResult root _name components _mo _po css images) = do
  let
    writeHtmlFile :: HtmlFile -> EitherT LoomSiteError IO ()
    writeHtmlFile (HtmlFile fp nav title h) = do
      safeIO $
        Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
      safeIO $
        TL.writeFile (out </> fp) . renderHtml . htmlTemplate prefix apx [css] (Just nav) title $ h
  safeIO $
    Dir.createDirectoryIfMissing True out
  generateLoomSiteStatic (LoomSiteRoot out)
  safeIO $
    copyFile (root </> renderCssFile css) (out </> cssAssetFilePath apx css)
  safeIO . for_ images $ \img ->
    copyFile (imageFilePath img) (out </> imageAssetFilePath apx img)
  for_ components $
    writeHtmlFile . loomComponentHtml
  writeHtmlFile $
    loomComponentsHtml components
  writeHtmlFile $
    loomHomeHtml

-- | Delete all the site files and then
cleanLoomSite :: LoomSiteRoot -> IO ()
cleanLoomSite (LoomSiteRoot out) =
  handleIf IO.Error.isDoesNotExistError (pure . const ()) $
    Dir.removeDirectoryRecursive out

-- | Generate the static files required for the loom site to function,
-- which are independent from the hosting project
generateLoomSiteStatic :: LoomSiteRoot -> EitherT LoomSiteError IO ()
generateLoomSiteStatic (LoomSiteRoot out) = do
  safeIO $
    Dir.createDirectoryIfMissing True out
  safeIO . for_ [loomCssFile, loomLogoFile] $ \(fp, b) -> do
    Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
    B.writeFile (out </> fp) b

--------

loomHomeHtml ::  HtmlFile
loomHomeHtml =
  HtmlFile "index.html" SiteHome (SiteTitle "Home") $
    H.div ! HA.class_ "loom-container-medium" $
      H.div ! HA.class_ "loom-vertical-grouping" $ do
        H.h1 ! HA.class_ "loom-h1" $ "Welcome to loom"
        H.img ! HA.src "https://cloud.githubusercontent.com/assets/355756/23049526/c99ade24-f510-11e6-851c-3e7902ed310c.jpg"

loomComponentsHtml ::  [Component] -> HtmlFile
loomComponentsHtml components =
  HtmlFile "components/index.html" SiteComponents (SiteTitle "Components") $
    H.div ! HA.class_ "loom-container-medium" $ do
      H.h1 ! HA.class_ "loom-h1 loom-page-header" $ "Components"
      H.ul ! HA.class_ "loom-ul loom-list-unstyled" $
        for_ components $ \c ->
          H.li $
            H.a ! HA.class_ "loom-a" ! HA.href (H.textValue . T.pack . componentDirectory $ c) $
              (H.text . componentName) c

loomComponentHtml :: Component -> HtmlFile
loomComponentHtml c =
  let
    root =
      HtmlFile (componentDirectory c </> "index.html") SiteComponents (SiteTitle $ componentName c <> " - Components") $
        H.div ! HA.class_ "loom-container-medium" $ do
          H.h1 ! HA.class_ "loom-h1 loom-page-header" $
            H.text . componentName $ c
          -- FIX Machinator interface
          -- FIX Examples
          -- FIX README.md?
          pure ()
  in
    -- FIX Pages will be generated here too as separate files
    root

renderHtmlErrorPage :: LoomSitePrefix -> AssetsPrefix -> Html -> Text
renderHtmlErrorPage spx apx =
  TL.toStrict . renderHtml . htmlTemplate spx apx [] Nothing (SiteTitle "Error")

loomSiteError :: Text -> Html
loomSiteError err =
  H.div ! HA.class_ "loom-container-medium" $ do
    H.h1 ! HA.class_ "loom-h1 loom-page-header" $ "Error"
    H.pre $ H.text err

loomSiteNotFound :: Html
loomSiteNotFound =
  H.div ! HA.class_ "loom-container-medium" $ do
    H.h1 ! HA.class_ "loom-h1 loom-page-header" $ "Error"
    H.p "Page not found"

htmlTemplate :: LoomSitePrefix -> AssetsPrefix -> [CssFile] -> Maybe SiteNavigation -> SiteTitle -> Html -> Html
htmlTemplate spx apx csss navm title body = do
  H.docType
  H.html $ do
    H.head $ do
      for_ csss $ \css ->
        H.link ! HA.rel "stylesheet" ! HA.href (H.textValue . cssAssetPath spx apx $ css)
      H.link ! HA.rel "stylesheet" ! HA.href (H.textValue . (<>) (loomSitePrefix spx) . T.pack . fst $ loomCssFile)
      H.title . H.text . renderSiteTitle $ title
    H.body $ do
      H.div ! HA.class_ "loom-pane-header" $
        H.div ! HA.class_ "loom-navigation-global" $
          H.div ! HA.class_ "loom-navigation-global__internal" $ do
            H.div ! HA.class_ "loom-logo-small" $
              H.a ! HA.class_ "loom-a" ! HA.href (H.textValue . loomSitePrefix $ spx) $
                H.img ! HA.class_ "loom-img"
                  ! HA.alt "Loom"
                  ! HA.src (H.textValue . (<>) (loomSitePrefix spx) . T.pack . fst $ loomLogoFile)
            for_ navm $ \nav ->
              H.nav ! H.customAttribute "role" "navigation" $
                H.ul ! HA.class_ "loom-ul loom-navigation-global__items" $ do
                  for_ [minBound..maxBound] $ \n ->
                    H.li ! HA.class_ (if n == nav then "loom-navigation-global__items__current" else "") $
                      case n of
                        SiteHome ->
                          H.a ! HA.class_ "loom-a" ! HA.href (H.textValue . loomSitePrefix $ spx) $ "Loom"
                        SiteComponents ->
                          H.a ! HA.class_ "loom-a" ! HA.href "/components" $ "Components"
      H.main ! HA.class_ "loom-pane-main" ! H.customAttribute "role" "main" $
        body

--------

loomCssFile :: (FilePath, ByteString)
loomCssFile =
  (,) "static/loom.css" $(embedFile "../loom-site/assets/loom.css")

loomLogoFile :: (FilePath, ByteString)
loomLogoFile =
  (,) "static/logo.svg" $(embedFile "../loom-site/assets/logo.svg")

--------

componentDirectory :: Component -> FilePath
componentDirectory c =
  "components" </> (T.unpack . componentName) c

safeIO :: IO a -> EitherT LoomSiteError IO a
safeIO =
  firstT LoomSiteFileError . newEitherT . tryIOError

copyFile :: FilePath -> FilePath -> IO ()
copyFile in' out = do
  Dir.createDirectoryIfMissing True . File.takeDirectory $ out
  Dir.copyFile in' out

renderLoomSiteError :: LoomSiteError -> Text
renderLoomSiteError le =
  case le of
    LoomSiteFileError e ->
      "Unknown file error " <> (T.pack . show) e
