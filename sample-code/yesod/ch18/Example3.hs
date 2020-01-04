#!/usr/bin/env stack
-- stack script --resolver lts-14.19
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import           Control.Monad.Logger
import           Data.Text               (Text)
import           Database.Persist.Sqlite
import           Yesod

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
|]

data App = App
    { persistConfig :: SqliteConf
    , connPool      :: ConnectionPool
    }
instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/blog/#BlogId BlogR GET
|]

getHomeR :: Handler Html
getHomeR = do
    blogs <- runDB $ rawSql
        "SELECT ??, ?? \
        \FROM blog INNER JOIN author \
        \ON blog.author=author.id"
        []

    defaultLayout $ do
        setTitle "Blog posts"
        [whamlet|
            <ul>
                $forall (Entity blogid blog, Entity _ author) <- blogs
                    <li>
                        <a href=@{BlogR blogid}>
                            #{blogTitle blog} by #{authorName author}
        |]

getBlogR :: BlogId -> Handler Html
getBlogR id = do 
          mBlog <- runDB $ get id
          defaultLayout $ do
            [whamlet|
              $maybe blog <- mBlog
                <p>Your title is #{blogTitle $ blog}
              $nothing
                <p>Hello
            |]

main :: IO ()
main = do
    -- Use an in-memory database with 1 connection. Terrible for production,
    -- but useful for testing.
    let conf = SqliteConf ":memory:" 1
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        -- Fill in some testing data
        alice <- insert $ Author "Alice"
        bob   <- insert $ Author "Bob"

        insert_ $ Blog alice "Alice's first post" "Hello World!"
        insert_ $ Blog bob "Bob's first post" "Hello World!!!"
        insert_ $ Blog alice "Alice's second post" "Goodbye World!"

    warp 3000 App
        { persistConfig = conf
        , connPool      = pool
        }
