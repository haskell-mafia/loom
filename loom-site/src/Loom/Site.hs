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
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import           Loom.Build.Core
import           Loom.Build.Data
import           Loom.Machinator (MachinatorOutput)
import           Loom.Projector (ProjectorOutput, ProjectorError, ProjectorInterpretError)
import qualified Loom.Projector as Projector
import qualified Loom.Projector as P
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
import qualified Text.Blaze.Internal as H
import qualified Text.Markdown as Markdown

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, newEitherT)

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
  | LoomSiteProjectorError ProjectorError
  | LoomSiteProjectorInterpretError ProjectorInterpretError
  deriving (Eq, Show)

data SiteComponent =
  SiteComponent {
      _siteComponentReadme :: Maybe Html
    -- FIX Machinator rendered not from disk
    , _siteComponentData :: [TL.Text]
    , _siteComponentExamples :: [(Text, Html)]
    , _siteComponentMocks :: [(Text, Html)]
    }

data HtmlFile =
    HtmlFile FilePath SiteNavigation SiteTitle Html
  | HtmlRawFile FilePath Html

defaultLoomSiteRoot :: Loom -> LoomSiteRoot
defaultLoomSiteRoot c =
  LoomSiteRoot $
    loomOutput c </> "site"

generateLoomSite :: LoomSitePrefix -> LoomSiteRoot -> AssetsPrefix -> LoomResult -> EitherT LoomSiteError IO ()
generateLoomSite prefix (LoomSiteRoot out) apx (LoomResult root _name components mo po css images) = do
  let
    writeHtmlFile :: HtmlFile -> EitherT LoomSiteError IO ()
    writeHtmlFile hf =
      case hf of
        HtmlFile fp nav title h -> do
          safeIO $
            Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
          safeIO $
            TL.writeFile (out </> fp) . renderHtml . htmlTemplate prefix apx [css] (Just nav) title $ h
        HtmlRawFile fp h -> do
          safeIO $
            Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
          safeIO $
            TL.writeFile (out </> fp) . renderHtml $ h
  safeIO $
    Dir.createDirectoryIfMissing True out
  generateLoomSiteStatic (LoomSiteRoot out)
  safeIO $
    copyFile (root </> renderCssFile css) (out </> cssAssetFilePath apx css)
  safeIO . for_ images $ \img ->
    copyFile (imageFilePath img) (out </> imageAssetFilePath apx img)
  for_ components $ \c -> do
    sc <- resolveSiteComponent mo po c
    mapM writeHtmlFile . loomComponentHtml prefix sc $ c
  writeHtmlFile $
    loomComponentsHtml prefix components
  writeHtmlFile $
    loomHomeHtml

resolveSiteComponent :: MachinatorOutput -> ProjectorOutput -> Component -> EitherT LoomSiteError IO SiteComponent
resolveSiteComponent mo po c =
  let
    root =
      loomFilePath . componentPath $ c
    -- FIX This is lazy, we should be rendering the in-memory version from machinator
    -- We just need to keep them associated with each component rather than in one blob
    loadData = do
      fs <- safeIO . getDirectoryContents $ root
      safeIO . fmap (join . catMaybes) . for (filter ((==) ".mcn" . File.takeExtension) fs) $
        -- Drop the version line
        fmap (fmap (List.dropWhile TL.null . drop 1 . TL.lines)) . readFileSafe
    loadDirectory d = do
      fs <- safeIO . getDirectoryContents $ root </> d
      fmap mconcat . for (filter ((==) ".prj" . File.takeExtension) fs) $ \f -> do
        po' <- firstT LoomSiteProjectorError $
          P.compileProjector
            (machinatorOutputToProjector mo)
            po
            (P.ProjectorInput (P.ModuleName "Ignore") root [f])
        for (join . Map.elems . Projector.projectorOutputModuleExprs $ po') $
          fmap ((,) (T.pack . File.takeBaseName $ f)) .
            fmap projectorHtmlToBlaze . hoistEither . first LoomSiteProjectorInterpretError .
              Projector.generateProjectorHtml (machinatorOutputToProjector mo) po
    loadReadme =
      fmap (Markdown.markdown Markdown.defaultMarkdownSettings)
        <$> readFileSafe (root </> "README.md")
  in
    SiteComponent
      <$> safeIO loadReadme
      <*> loadData
      <*> loadDirectory "example"
      <*> loadDirectory "mock"

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

loomComponentsHtml :: LoomSitePrefix -> [Component] -> HtmlFile
loomComponentsHtml spx components =
  HtmlFile "components/index.html" SiteComponents (SiteTitle "Components") $
    H.div ! HA.class_ "loom-container-medium" $ do
      H.h1 ! HA.class_ "loom-h1 loom-page-header" $ "Components"
      H.ul ! HA.class_ "loom-ul loom-list-unstyled" $
        for_ components $ \c ->
          H.li $
            H.a ! HA.class_ "loom-a" ! HA.href (H.textValue . mappend (loomSitePrefix spx) . T.pack . componentDirectory $ c) $
              (H.text . componentName) c

loomComponentHtml :: LoomSitePrefix -> SiteComponent -> Component -> [HtmlFile]
loomComponentHtml spx (SiteComponent rm d es ps) c =
  let
    pageLink n =
      componentDirectory c </> "pages" </> T.unpack n <> ".html"
    root =
      HtmlFile
        (componentDirectory c </> "index.html")
        SiteComponents
        (SiteTitle $ componentName c <> " - Components")
        $
          H.div ! HA.class_ "loom-container-wide" $ do
            H.h1 ! HA.class_ "loom-h1 loom-page-header" $
              H.text . componentName $ c
            for_ rm $
              H.p
            unless (null d) $
              H.div ! HA.class_ "loom-vertical-grouping" $ do
                H.h2 ! HA.class_ "loom-h2" $ "Interface"
                H.pre . H.lazyText $ TL.unlines d
            unless (null ps) $
              H.div ! HA.class_ "loom-vertical-grouping" $ do
                H.h2 ! HA.class_ "loom-h2" $ "Mocks"
                for_ ps $ \(n, _) ->
                  H.div ! HA.class_ "loom-vertical-grouping-xs" $
                    H.a ! HA.class_ "loom-a loom-btn-link-primary"
                      ! HA.href (H.textValue . (<>) (loomSitePrefix spx) . T.pack . pageLink $ n)
                      $ H.text n
            for_ es $ \(n, e) -> do
              H.h2 ! HA.class_ "loom-h2" $ H.text n
              H.div ! HA.class_ "loom-vertical-grouping" $
                H.div ! HA.class_ "loom-pullout" $ e
    pages =
      with ps $ \(n, p) ->
        HtmlRawFile (pageLink n) p
  in
    root : pages

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
                          H.a ! HA.class_ "loom-a" ! HA.href (H.textValue $ loomSitePrefix spx <> "components") $ "Components"
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

projectorHtmlToBlaze :: P.Html -> Html
projectorHtmlToBlaze h =
  case h of
    P.Plain t ->
      H.text t
    P.Raw t ->
      H.preEscapedToHtml t
    P.Whitespace t ->
      H.text t
    P.Comment t ->
      H.textComment t
    P.Element t ats b ->
      H.customParent (H.textTag t) (projectorHtmlToBlaze b)
        ! (foldMap (\(P.Attribute k v) -> H.customAttribute (H.textTag k) (H.toValue v)) ats)
    P.VoidElement t ats ->
      H.customLeaf (H.textTag t) True
        ! (foldMap (\(P.Attribute k v) -> H.customAttribute (H.textTag k) (H.toValue v)) ats)
    P.Nested hs ->
      foldMap projectorHtmlToBlaze hs

--------

componentDirectory :: Component -> FilePath
componentDirectory c =
  "components" </> (T.unpack . componentName) c

readFileSafe :: FilePath -> IO (Maybe TL.Text)
readFileSafe =
  handleIf IO.Error.isDoesNotExistError (pure . const Nothing) .
    fmap Just . TL.readFile

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents dir =
  handleIf IO.Error.isDoesNotExistError (pure . const []) .
    fmap (fmap (dir </>) . filter (not . flip elem [".", ".."])) . Dir.getDirectoryContents $ dir

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
    LoomSiteProjectorError e ->
      Projector.renderProjectorError e
    LoomSiteProjectorInterpretError e ->
      Projector.renderProjectorInterpretError e
