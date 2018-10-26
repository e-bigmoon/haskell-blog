#!/usr/bin/env stack
-- stack script --resolver lts-12.14

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
import           Control.Monad.Logger    (runNoLoggingT)
import           Data.Text               (Text)
import           Data.Time
import           Database.Persist.Sqlite
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Link
    title Text
    url Text
    added UTCTime
|]

newtype App = App ConnectionPool

mkYesod "App" [parseRoutes|
/         HomeR    GET
/add-link AddLinkR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB db = do
        App pool <- getYesod
        runSqlPool db pool

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form method=post action=@{AddLinkR}>
            <p>
                Add a new link to
                <input type=url name=url value=http://>
                titled
                <input type=text name=title>
                <input type=submit value="Add link">
        <h2>Existing links
        ^{existingLinks}
    |]

existingLinks :: Widget
existingLinks = do
    links <- handlerToWidget $ runDB $ selectList [] [LimitTo 5, Desc LinkAdded]
    [whamlet|
        <ul>
            $forall Entity _ link <- links
                <li>
                    <a href=#{linkUrl link}>#{linkTitle link}
    |]

postAddLinkR :: Handler ()
postAddLinkR = do
    url <- runInputPost $ ireq urlField "url"
    title <- runInputPost $ ireq textField "title"
    now <- liftIO getCurrentTime
    runDB $ insert $ Link title url now
    setMessage "Link added"
    redirect HomeR

main :: IO ()
main = runNoLoggingT $ withSqlitePool "links.sqlite" 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ App pool
