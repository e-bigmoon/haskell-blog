{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
import           Control.Monad              (join)
import           Control.Monad.Catch        (throwM)
import           Control.Monad.CryptoRandom
import           Control.Monad.Error.Class  (MonadError (..))
import           Crypto.Random              (SystemRandom, newGenIO)
import           Data.ByteString.Base16     (encode)
import           Data.IORef
import           Data.Text.Encoding         (decodeUtf8)
import           UnliftIO.Exception         (catch)
import           Yesod

newtype App = App
    { randGen :: IORef SystemRandom
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = do
    randomBS <- getBytes 16
    defaultLayout
        [whamlet|
            <p>Here's some random data: #{decodeUtf8 $ encode randomBS}
        |]

instance MonadError GenError Handler where
    throwError = throwM
    catchError = catch
instance MonadCRandom GenError Handler where
    getCRandom  = wrap crandom
    {-# INLINE getCRandom #-}
    getBytes i = wrap (genBytes i)
    {-# INLINE getBytes #-}
    getBytesWithEntropy i e = wrap (genBytesWithEntropy i e)
    {-# INLINE getBytesWithEntropy #-}
    doReseed bs = do
        genRef <- fmap randGen getYesod
        join $ liftIO $ atomicModifyIORef genRef $ \gen ->
            case reseed bs gen of
                Left e     -> (gen, throwM e)
                Right gen' -> (gen', return ())
    {-# INLINE doReseed #-}

wrap :: (SystemRandom -> Either GenError (a, SystemRandom)) -> Handler a
wrap f = do
    genRef <- fmap randGen getYesod
    join $ liftIO $ atomicModifyIORef genRef $ \gen ->
        case f gen of
            Left e          -> (gen, throwM e)
            Right (x, gen') -> (gen', return x)

main :: IO ()
main = do
    gen <- newGenIO
    genRef <- newIORef gen
    warp 3000 App
        { randGen = genRef
        }
