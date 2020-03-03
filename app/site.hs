{-# LANGUAGE OverloadedStrings #-}

import qualified Config as C
import Control.Lens ((^.))
import Data.List (stripPrefix)
import Data.Maybe
import HTMLEntities.Text
import Hakyll hiding (dateFieldWith)
import Hakyll.Ext
import Hakyll.Web.Sass (sassCompiler)
import RIO hiding ((^.))
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as Text
import System.FilePath (takeBaseName, takeDirectory, takeFileName)

main :: IO ()
main = do
  msiteConfig <- C.fromConfig "config.yml"
  either (error "Expected file 'config.yml' not found") main' msiteConfig

main' :: C.Site -> IO ()
main' siteConfig = hakyllWith hakyllConfig $ do
  match (fromGlob "images/**" .||. fromGlob "js/**" .||. fromGlob "lib/**") $ do
    route idRoute
    compile copyFileCompiler
  match "css/*.scss" $ do
    route $ setExtension "css"
    compile (fmap compressCss <$> sassCompiler)
  match (fromGlob "pages/**.md") $ do
    route $
      customRoute
        ( fromMaybe (error "Expected pages to be in 'pages' folder")
            . stripPrefix "pages/"
            . toFilePath
        )
        `composeRoutes` setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" siteCtx
        >>= loadAndApplyTemplate "templates/default.html" siteCtx
        >>= relativizeUrls
  tags <- buildTags "posts/**" (fromCapture "tags/*.html")
  createTagsRules tags (\xs -> "Posts tagged \"" ++ xs ++ "\"")
  categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
  createTagsRules categories (\xs -> "Posts categorised as \"" ++ xs ++ "\"")
  postIDs <- sortChronological' =<< getMatches "posts/**"
  let prevPosts = Nothing : map Just postIDs
      nextPosts = L'.tail $ map Just postIDs ++ [Nothing]
  forM_ (L.zip3 postIDs prevPosts nextPosts) $
    \(postID, mprevPost, mnextPost) -> create [postID] $ do
      let prevPageCtx = case mprevPost of
            Just i ->
              field "previousPageUrl" (\_ -> pageUrl i)
                <> field "previousPageTitle" (\_ -> pageTitle i)
            _ -> mempty
          nextPageCtx = case mnextPost of
            Just i ->
              field "nextPageUrl" (\_ -> pageUrl i)
                <> field "nextPageTitle" (\_ -> pageTitle i)
            _ -> mempty
          namedTags = [("tags", tags), ("categories", categories)]
          ctx = mconcat [prevPageCtx, nextPageCtx, postCtx]
      route $ setExtension "html"
      compile $
        pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate
            "templates/post.html"
            (ctxWithTags ctx namedTags)
          >>= loadAndApplyTemplate
            "templates/default.html"
            (ctxWithTags ctx namedTags)
          >>= relativizeUrls
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst' =<< loadAll "posts/**"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` siteCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  match "pages/index.html" $ do
    route (constRoute "index.html")
    compile $ do
      posts <- recentFirst' =<< loadAll "posts/**"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "BIGMOON haskellers blog"
              <> siteCtx
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
  match (fromGlob "partials/*" .||. fromGlob "templates/*") $
    compile templateBodyCompiler
  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedConfig = siteConfig ^. #feed
          feedCtx = mapContext (Text.unpack . text . Text.pack) postCtx <> bodyField "description"
      posts <-
        fmap (take 10) . recentFirst'
          =<< loadAllSnapshots
            "posts/**"
            "content"
      renderAtom (atomFeedConfiguration feedConfig) feedCtx posts
  -- SEO-related stuff
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst' =<< loadAll "posts/**"
      pages <- loadAll "pages/*"
      let crawlPages = sitemapPages pages ++ posts
          sitemapCtx =
            mconcat [listField "entries" siteCtx (return crawlPages), siteCtx]
      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
        >>= relativizeUrls
  match "robots.txt" $ do
    route idRoute
    compile $ getResourceBody >>= relativizeUrls
  where
    ctxWithTags :: Context String -> [(String, Tags)] -> Context String
    ctxWithTags =
      foldr (\(name, tags) baseCtx -> tagsField name tags <> baseCtx)
    siteCtx :: Context String
    siteCtx = generalCtx <> styleCtx <> defaultContext
      where
        generalCtx =
          field "site-title" (toField #general #siteTitle)
            <> field "head-title" (toField #general #headTitle)
            <> field "base-url" (toField #general #baseUrl)
        styleCtx =
          mconcat
            [ field "header-colour" (toField #style #headerColour),
              field "head-theme-colour" (toField #style #headThemeColour),
              field "footer-colour" (toField #style #footerColour),
              field "footer-btn-colour" (toField #style #footerBtnColour),
              field "footer-link-colour" (toField #style #footerLinkColour),
              field
                "navbar-text-colour-desktop"
                (toField #style #navbarTextColourDesktop),
              field
                "navbar-text-colour-mobile"
                (toField #style #navbarTextColourMobile),
              field "share-button-colour" (toField #style #shareButtonColour),
              field
                "share-button-small-colour"
                (toField #style #shareButtonSmallColour)
            ]
        toField configObj configField item = do
          _metadata <- getMetadata $ itemIdentifier item
          return $ siteConfig ^. configObj ^. configField
    postCtx :: Context String
    postCtx =
      mconcat
        [ dateField' "date" "%B %e, %Y",
          dateField' "published" "%Y-%m-%dT%H:%M:%SZ",
          dateField' "updated" "%Y-%m-%dT%H:%M:%SZ",
          teaserField "teaser" "content",
          mapContext
            (trim' . take 160 . stripTags)
            (teaserField "teaser-short" "content"),
          siteCtx
        ]
      where
        trim' xs = map snd . filter trim'' $ zip [0 ..] xs
          where
            trim'' (ix, x)
              | ix == 0 || ix == (length xs - 1) = x `notElem` [' ', '\n', '\t']
              | otherwise = True
    createTagsRules :: Tags -> (String -> String) -> Rules ()
    createTagsRules tags mkTitle = tagsRules tags $ \tag pattern' -> do
      route idRoute
      compile $ do
        posts <- recentFirst' =<< loadAll pattern'
        let ctx =
              constField "title" (mkTitle tag)
                <> listField "posts" postCtx (return posts)
                <> siteCtx
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
    pageTitle, pageUrl :: Identifier -> Compiler String
    pageTitle i = do
      mtitle <- getMetadataField i "title"
      case mtitle of
        Just title -> return title
        Nothing -> fail "no 'title' field"
    pageUrl i = do
      mfilePath <- getRoute i
      case mfilePath of
        Just filePath -> return (toUrl filePath)
        Nothing -> fail "no route"

atomFeedConfiguration :: C.Feed -> FeedConfiguration
atomFeedConfiguration fs =
  FeedConfiguration
    { feedTitle = fs ^. #title,
      feedDescription = fs ^. #description,
      feedAuthorName = fs ^. #authorName,
      feedAuthorEmail = fs ^. #authorEmail,
      feedRoot = fs ^. #root
    }

-- Friendlier config when using docker
hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration {previewHost = "0.0.0.0", previewPort = 3001}

sitemapPages :: [Item String] -> [Item String]
sitemapPages = filter ((/= "pages/LICENSE.md") . toFilePath . itemIdentifier)

-- | Helper Functions for dirctory design is yyyy/mm-dd-xxx.md
toDate :: Identifier -> String
toDate ident = yyyy ++ "-" ++ mmdd
  where
    path = toFilePath ident
    yyyy = takeFileName $ takeDirectory path
    mmdd = take 5 $ takeBaseName path

dateField' :: String -> String -> Context a
dateField' = dateFieldWith toDate

recentFirst' :: MonadMetadata m => [Item a] -> m [Item a]
recentFirst' = recentFirstWith toDate

sortChronological' :: MonadMetadata m => [Identifier] -> m [Identifier]
sortChronological' = sortChronologicalWith toDate
