{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Ext where

import Data.List
import Data.Time
import Hakyll
import RIO
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

dateFieldWith :: (Identifier -> String) -> String -> String -> Context a
dateFieldWith f key format = field key $ \item -> do
  time <- getItemUTCWith f defaultTimeLocale $ itemIdentifier item
  return $ formatTime defaultTimeLocale format time

getItemUTCWith ::
  MonadMetadata m =>
  (Identifier -> String) ->
  TimeLocale ->
  Identifier ->
  m UTCTime
getItemUTCWith f locale ident =
  pure $ parseTimeOrError True locale "%Y-%m-%d" (f ident)

chronologicalWith ::
  MonadMetadata m => (Identifier -> String) -> [Item a] -> m [Item a]
chronologicalWith f =
  sortByM $ getItemUTCWith f defaultTimeLocale . itemIdentifier

recentFirstWith ::
  MonadMetadata m => (Identifier -> String) -> [Item a] -> m [Item a]
recentFirstWith f = fmap reverse . chronologicalWith f

sortChronologicalWith ::
  MonadMetadata m => (Identifier -> String) -> [Identifier] -> m [Identifier]
sortChronologicalWith f =
  fmap (fmap itemIdentifier) . chronologicalWith f . fmap (flip Item ())

sortRecentFirstWith ::
  MonadMetadata m => (Identifier -> String) -> [Identifier] -> m [Identifier]
sortRecentFirstWith f =
  fmap (fmap itemIdentifier) . recentFirstWith f . fmap (flip Item ())

tagsFieldWithSep :: H.Html -> String -> Tags -> Context a
tagsFieldWithSep sep =
  tagsFieldWith getTags simpleRenderLink (mconcat . intersperse sep)

sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
sortByM f = fmap (map fst . sortOn snd) . mapM (fmap <$> (,) <*> f)

simpleRenderLink :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink tag =
  fmap (\path -> H.a ! A.href (toValue $ toUrl path) $ toHtml tag)
