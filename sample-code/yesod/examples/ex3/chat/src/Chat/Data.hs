-- @Chat/Data.hs
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Chat.Data where

import           Blaze.ByteString.Builder.Char.Utf8  (fromText)
import           Control.Concurrent.Chan
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import           Network.Wai.EventSource
import           Network.Wai.EventSource.EventStream
import           Yesod
import           Yesod.Core.Types (SubHandlerFor)

-- | Our subsite foundation. We keep a channel of events that all connections
-- will share.
data Chat = Chat (Chan ServerEvent)

mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

class (Yesod master, RenderMessage master FormMessage)
        => YesodChat master where
    getUserName :: HandlerFor master Text
    isLoggedIn :: HandlerFor master Bool

type ChatHandler a =
    forall master. YesodChat master =>
    SubHandlerFor Chat master a

postSendR :: ChatHandler ()
postSendR = do
    from <- liftHandler getUserName
    body <- runInputGet $ ireq textField "message"
    Chat chan <- getSubYesod
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from <> fromText ": " <> fromText body

getReceiveR :: ChatHandler ()
getReceiveR = do
    Chat chan <- getSubYesod
    sendWaiApplication $ eventSourceAppChan chan
