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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import           Loom.Build.Assets
import           Loom.Build.Core
import           Loom.Core.Data
import           Loom.Machinator (MachinatorOutput (..))
import           Loom.Projector (ProjectorOutput, ProjectorError, ProjectorInterpretError)
import qualified Loom.Projector as Projector
import qualified Loom.Projector as P

import qualified Machinator.Core.Data.Definition as MC
import qualified Machinator.Core.Pretty as MP

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
  | SiteHow
  | SiteComponents
  | SiteData
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
  | HtmlRawFile FilePath SiteTitle Html

defaultLoomSiteRoot :: FilePath -> LoomSiteRoot
defaultLoomSiteRoot c =
  LoomSiteRoot $
    c </> "site"

generateLoomSite :: LoomSitePrefix -> LoomSiteRoot -> AssetsPrefix -> LoomResult -> EitherT LoomSiteError IO ()
generateLoomSite prefix root@(LoomSiteRoot out) apx (LoomResult _name components mo po cssIn images js) = do
  let
    css =
      CssFile . File.takeFileName . renderCssFile $ cssIn
    writeHtmlFile' :: HtmlFile -> EitherT LoomSiteError IO ()
    writeHtmlFile' =
      writeHtmlFile prefix root [(CssFile . cssAssetFilePath apx) css]
  safeIO $
    Dir.createDirectoryIfMissing True out
  generateLoomSiteStatic prefix (LoomSiteRoot out)
  safeIO $
    prefixCssImageAssets prefix apx images
      (CssFile $ out </> cssAssetFilePath apx css)
      cssIn
  safeIO . for_ images $ \img ->
    copyFile (imageFilePath img) (out </> imageAssetFilePath apx img)
  safeIO . for_ js $ \j ->
    copyFile (renderJsFile j) (out </> jsAssetFilePath apx j)
  void . flip Map.traverseWithKey components $ \ln cs ->
    for_ cs $ \c -> do
      sc <- resolveSiteComponent prefix apx [css] images mo po c
      mapM writeHtmlFile' . loomComponentHtml prefix sc ln $ c
  writeHtmlFile' $
    loomComponentsHtml prefix components
  writeHtmlFile' $
    loomDataHtml mo

resolveSiteComponent ::
     LoomSitePrefix
  -> AssetsPrefix
  -> [CssFile]
  -> [ImageFile]
  -> MachinatorOutput
  -> ProjectorOutput
  -> Component
  -> EitherT LoomSiteError IO SiteComponent
resolveSiteComponent spfx apfx css images mo po c =
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
            (P.ProjectorInput "ignore" root images [f])
        for (join . Map.elems . Projector.projectorOutputModuleExprs $ po') $
          fmap ((,) (T.pack . File.takeBaseName $ f)) .
            fmap projectorHtmlToBlaze . hoistEither . first LoomSiteProjectorInterpretError .
              Projector.generateProjectorHtml (machinatorOutputToProjector mo) spfx apfx css images po
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
generateLoomSiteStatic :: LoomSitePrefix -> LoomSiteRoot -> EitherT LoomSiteError IO ()
generateLoomSiteStatic prefix root@(LoomSiteRoot out) = do
  safeIO $
    Dir.createDirectoryIfMissing True out
  safeIO . for_ ([loomCssFile, loomLogoFile] <> loomLogoFavicons) $ \(fp, b) -> do
    Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
    B.writeFile (out </> fp) b
  writeHtmlFile prefix root [] $
    loomHomeHtml
  writeHtmlFile prefix root [] $
    loomHowHtml prefix
  for_ loomHowHtmls $
    writeHtmlFile prefix root []

writeHtmlFile :: LoomSitePrefix -> LoomSiteRoot -> [CssFile] -> HtmlFile -> EitherT LoomSiteError IO ()
writeHtmlFile prefix (LoomSiteRoot out) css hf =
  case hf of
    HtmlFile fp nav title h -> do
      safeIO $
        Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
      safeIO $
        TL.writeFile (out </> fp) . renderHtml $
          htmlTemplate prefix (css <> [(CssFile . fst) loomCssFile]) (Just nav) title h
    HtmlRawFile fp title h -> do
      safeIO $
        Dir.createDirectoryIfMissing True . File.takeDirectory $ out </> fp
      safeIO $
        TL.writeFile (out </> fp) . renderHtml $
          htmlRawTemplate prefix css title h

--------

loomHomeHtml ::  HtmlFile
loomHomeHtml =
  HtmlFile "index.html" SiteHome (SiteTitle "Home") $
    H.div ! HA.class_ "loom-container-medium" $
      H.div ! HA.class_ "loom-vertical-grouping" $ do
        H.h1 ! HA.class_ "loom-h1" $ "Welcome to loom"
        H.img ! HA.src "https://cloud.githubusercontent.com/assets/355756/23049526/c99ade24-f510-11e6-851c-3e7902ed310c.jpg"

loomHowHtml :: LoomSitePrefix -> HtmlFile
loomHowHtml spx =
  HtmlFile "how/index.html" SiteHow (SiteTitle "How to Use") $
    H.div ! HA.class_ "loom-container-medium" $
      H.div ! HA.class_ "loom-vertical-grouping" $ do
        H.h1 ! HA.class_ "loom-h1" $ "How to Use"

        H.h2 ! HA.class_ "loom-h2" $
          H.a ! HA.class_ "loom-a" ! href spx "how/component.html" $
            "Creating New Components"
        H.p $ "How to create a component"

        H.h2 ! HA.class_ "loom-h2" $
          H.a ! HA.class_ "loom-a" ! href spx "how/projector.html" $
            "Projector"
        H.p $ "The projector reference guide"

        H.h2 ! HA.class_ "loom-h2" $
          H.a ! HA.class_ "loom-a" ! href spx "how/machinator.html" $
            "Machinator"
        H.p $ "The machinator reference guide"

loomHowHtmls :: [HtmlFile]
loomHowHtmls =
  let
    snippets =
      [
          ("component.html", "Creating New Components", howComponentSnippet)
        , ("machinator.html", "Machinator", howMachinatorSnippet)
        , ("projector.html", "Projector", howProjectorSnippet)
        ]
  in
    with snippets $ \(f, t, b) ->
      HtmlFile ("how" </> f) SiteHow (SiteTitle (t <> " - How to Use")) $
        H.div ! HA.class_ "loom-container-medium" $ do
          H.h1 ! HA.class_ "loom-h1 loom-page-header" $ H.text t
          H.div ! HA.class_ "loom" $
            Markdown.markdown Markdown.defaultMarkdownSettings . TL.fromStrict $ b

loomComponentsHtml :: LoomSitePrefix -> Map LoomName [Component] -> HtmlFile
loomComponentsHtml spx components =
  HtmlFile "components/index.html" SiteComponents (SiteTitle "Components") $
    H.div ! HA.class_ "loom-container-medium" $ do
      H.h1 ! HA.class_ "loom-h1 loom-page-header" $ "Components"
      H.ul ! HA.class_ "loom-ul loom-list-unstyled" $
        void . flip Map.traverseWithKey components $ \ln cs -> do
          H.h2 ! HA.class_ "loom-h2" $
            H.a ! HA.id (H.textValue (renderLoomName ln)) $ H.text (renderLoomName ln)
          for_ cs $ \c ->
            let pageLink = T.unpack (loomSitePrefix spx) </> componentDirectory c ln in
            H.li $
              H.a ! HA.class_ "loom-a" ! HA.href (H.textValue (T.pack pageLink)) $
                H.text (templateName ln c)

loomComponentHtml :: LoomSitePrefix -> SiteComponent -> LoomName -> Component -> [HtmlFile]
loomComponentHtml spx (SiteComponent rm d es ps) ln c =
  let
    pageLink n =
      componentDirectory c ln </> "pages" </> T.unpack n <> ".html"
    root =
      HtmlFile
        (componentDirectory c ln </> "index.html")
        SiteComponents
        (SiteTitle $ componentName c <> " - Components")
        $
          H.div ! HA.class_ "loom-container-wide" $ do
            H.h1 ! HA.class_ "loom-h1 loom-page-header" $
              H.text (templateName ln c)
            for_ rm $
              H.p
            unless (null d) $
              H.div ! HA.class_ "loom-vertical-grouping" $ do
                H.h2 ! HA.class_ "loom-h2" $ "Data Types"
                H.div ! HA.class_ "loom-pullout" $
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
        HtmlRawFile (pageLink n) (SiteTitle n) p
  in
    root : pages

loomDataHtml :: MachinatorOutput -> HtmlFile
loomDataHtml (MachinatorOutput defmap) =
  HtmlFile "data/index.html" SiteData (SiteTitle "Data Types") $
    H.div ! HA.class_ "loom-container-medium" $ do
      H.h1 ! HA.class_ "loom-h1 loom-page-header" $ "Data Types"
      -- Would be nice if these were grouped by loom name before they arrive here
      let sorted = sortOn MC.defName (mconcat (Map.elems defmap))
      for_ sorted $ \d@(MC.Definition (MC.Name name) _def) -> do
        H.a ! HA.href (H.textValue ("#" <> anchorDefinition name)) $
          H.h2 ! HA.id (H.textValue (anchorDefinition name)) $ (H.text name)
        H.div ! HA.class_ "loom-pullout loom-code" $
          loomDataDefinition d

loomDataDefinition :: MC.Definition -> Html
loomDataDefinition def =
  H.preEscapedText $
    MP.ppDefinitionAnnotated start end def
  where
    start ann =
      case ann of
        MP.Punctuation ->
          "<span class=\"loom-code-punctuation\">"
        MP.Keyword ->
          "<span class=\"loom-code-keyword\">"
        MP.Primitive ->
          "<span class=\"loom-code-primitive\">"
        MP.TypeDefinition _ ->
          "<span class=\"loom-code-type-definition\">"
        MP.TypeUsage n ->
          "<a href=\"#" <> anchorDefinition n <> "\"><span class=\"loom-code-type-usage\">"
        MP.ConstructorDefinition c ->
          "<span id=\"" <> anchorConstructor c <> "\" class=\"loom-code-constructor-definition\">"
        MP.FieldDefinition t f ->
          "<span id=\"" <> anchorField t f <> "\" class=\"loom-code-field-definition\">"
        MP.VersionMarker ->
          ""
    end ann =
      case ann of
        MP.TypeUsage _ ->
          "</span></a>"
        _ ->
          "</span>"

anchorDefinition :: Text -> Text
anchorDefinition n =
  "t:" <> n

anchorConstructor :: Text -> Text
anchorConstructor c =
  "c:" <> c

anchorField :: Text -> Text -> Text
anchorField t f =
  "f:" <> t <> "-" <> f

renderHtmlErrorPage :: LoomSitePrefix -> Html -> Text
renderHtmlErrorPage spx =
  TL.toStrict . renderHtml . htmlTemplate spx [(CssFile . fst) loomCssFile] (Just SiteComponents) (SiteTitle "Error")

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

htmlTemplate :: LoomSitePrefix -> [CssFile] -> Maybe SiteNavigation -> SiteTitle -> Html -> Html
htmlTemplate spx css navm title body =
  htmlRawTemplate spx css title $ do
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
                      SiteHow ->
                        H.a ! HA.class_ "loom-a" ! HA.href (H.textValue $ loomSitePrefix spx <> "how") $ "How to Use"
                      SiteComponents ->
                        H.a ! HA.class_ "loom-a" ! HA.href (H.textValue $ loomSitePrefix spx <> "components") $ "Components"
                      SiteData ->
                        H.a ! HA.class_ "loom-a" ! HA.href (H.textValue $ loomSitePrefix spx <> "data") $ "Data Types"
    H.main ! HA.class_ "loom-pane-main" ! H.customAttribute "role" "main" $
      body

htmlRawTemplate :: LoomSitePrefix -> [CssFile] -> SiteTitle -> Html -> Html
htmlRawTemplate spx csss title body = do
  H.docType
  H.html $ do
    H.head $ do
      H.title . H.text . renderSiteTitle $ title

      for_ csss $ \css ->
        H.link ! HA.rel "stylesheet" ! HA.href (H.textValue . (<>) (loomSitePrefix spx) . T.pack . renderCssFile $ css)

      H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.sizes "96x96" ! href spx "static/favicon-96x96.png"
      H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.sizes "32x32" ! href spx "static/favicon-32x32.png"
      H.link ! HA.rel "icon" ! HA.type_ "image/png" ! HA.sizes "16x16" ! href spx "static/favicon-16x16.png"
      H.link ! HA.rel "icon" ! href spx "static/favicon.ico"

    H.body $
      body

href :: LoomSitePrefix -> FilePath -> H.Attribute
href spx =
  HA.href . H.textValue . (<>) (loomSitePrefix spx) . T.pack

--------

loomCssFile :: (FilePath, ByteString)
loomCssFile =
  (,) "static/loom.css" $(embedFile "../loom-site/assets/loom.css")

loomLogoFile :: (FilePath, ByteString)
loomLogoFile =
  (,) "static/logo.svg" $(embedFile "../loom-site/assets/logo.svg")

loomLogoFavicons :: [(FilePath, ByteString)]
loomLogoFavicons =
  [
      (,) "static/favicon.ico" $(embedFile "../loom-site/assets/favicon.ico")
    , (,) "static/favicon-16x16.png" $(embedFile "../loom-site/assets/favicon-16x16.png")
    , (,) "static/favicon-32x32.png" $(embedFile "../loom-site/assets/favicon-32x32.png")
    , (,) "static/favicon-96x96.png" $(embedFile "../loom-site/assets/favicon-96x96.png")
    ]

howComponentSnippet :: Text
howComponentSnippet =
  T.decodeUtf8 $(embedFile "../loom-site/how/component.md")

howMachinatorSnippet :: Text
howMachinatorSnippet =
  T.decodeUtf8 $(embedFile "../loom-site/how/machinator.md")

howProjectorSnippet :: Text
howProjectorSnippet =
  T.decodeUtf8 $(embedFile "../loom-site/how/projector.md")

--------

projectorHtmlToBlaze :: P.Html -> Html
projectorHtmlToBlaze h =
  case h of
    P.Plain t ->
      H.text t
    P.Raw t ->
      H.preEscapedToHtml t
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

componentDirectory :: Component -> LoomName -> FilePath
componentDirectory c (LoomName ln) =
  "components" </> T.unpack ln </> (T.unpack . componentName) c

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
