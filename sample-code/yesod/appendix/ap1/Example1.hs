#!/usr/bin/env stack
-- stack script --resolver lts-17.4
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
import Control.Exception (mask, onException)
--import Control.Exception.Control (mask)
import Control.Concurrent.MVar (MVar, addMVarFinalizer, mkWeakMVar, newMVar, putMVar,takeMVar)
import Data.Bifunctor (first, bimap)
import Data.Function ((&))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Monoid (mempty)
import Control.Arrow ((***))
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode(..), hPutStrLn, withFile)

-- type : ErrorT, ReaderT, WriterT, StateT
-- instance : Functor m , Applicative m, Monad m, MonadTrans (t w), MonadTransControl (t r), MonadIO (t r m), MonadControlIO (t r m)
-- MonadTransControl (ReaderT r) cannnot be defined.
-- MonadTransControl (StateT s) cannnot be defined.

------- func
data MyError = MyError deriving Show

throwError :: (Monad m) => e -> ErrorT e m a
throwError l = ErrorT $ return (Left l)

withMyFile :: (Handle -> IO a) -> IO a
withMyFile = withFile "test.txt" WriteMode

sayHi :: Handle -> IO ()
sayHi handle = hPutStrLn handle "Hi there"

useMyFile :: IO ()
useMyFile = withMyFile sayHi

sayHiError :: Handle -> ErrorT MyError IO ()
sayHiError handle = do
    lift $ hPutStrLn handle "Hi there, error!"
    throwError MyError

useMyFileError1 :: ErrorT MyError IO ()
useMyFileError1 =
    let unwrapped :: Handle -> IO (Either MyError ())
        unwrapped handle = runErrorT $ sayHiError handle
        applied :: IO (Either MyError ())
        applied = withMyFile unwrapped
        rewrapped :: ErrorT MyError IO ()
        rewrapped = ErrorT applied
     in rewrapped

errorRun :: Run (ErrorT MyError)
errorRun = undefined

-- undefefined is used for definition of errorRun
useMyFileError2 :: IO (ErrorT MyError Identity ())
useMyFileError2 =
    let afterRun :: Handle -> IO (ErrorT MyError Identity ())
        afterRun handle = errorRun $ sayHiError handle
        applied :: IO (ErrorT MyError Identity ())
        applied = withMyFile afterRun
     in applied

useMyFileError3 :: Monad m => ErrorT MyError IO (ErrorT MyError m ())
useMyFileError3 =
    liftControl inside
    where 
      inside :: Monad m => Run (ErrorT MyError) -> IO (ErrorT MyError m ())
      inside run = withMyFile $ helper run
      helper :: Monad m => Run (ErrorT MyError) -> Handle -> IO (ErrorT MyError m ())
      helper run handle = run (sayHiError handle :: ErrorT MyError IO ())

useMyFileError4 :: ErrorT MyError IO ()
useMyFileError4 = join useMyFileError3

useMyFileError5 :: ErrorT MyError IO ()
useMyFileError5 =
    control inside
  where
    inside :: Monad m => Run (ErrorT MyError) -> IO (ErrorT MyError m ())
    inside run = withMyFile $ helper run
    helper :: Monad m
           => Run (ErrorT MyError) -> Handle -> IO (ErrorT MyError m ())
    helper run handle = run (sayHiError handle :: ErrorT MyError IO ())

useMyFileError6 :: ErrorT MyError IO ()
useMyFileError6 = control $ \run -> withMyFile $ run . sayHiError

useMyFileError7 :: ErrorT MyError IO ()
useMyFileError7 = controlIO $ \run -> withMyFile $ run . sayHiError

sayHiCrazy :: Handle -> ReaderT Int (StateT Double (ErrorT MyError IO)) ()
sayHiCrazy handle = liftIO $ hPutStrLn handle "Madness!"

useMyFileCrazy :: ReaderT Int (StateT Double (ErrorT MyError IO)) ()
useMyFileCrazy = controlIO $ \run -> withMyFile $ run . sayHiCrazy

onExceptionError :: ErrorT MyError IO a -> ErrorT MyError IO b -> ErrorT MyError IO a
onExceptionError action after = controlIO $ \run -> run action `onException` run after

addMVarFinalizerError :: MVar a -> ErrorT MyError IO () -> ErrorT MyError IO ()
addMVarFinalizerError mvar f = controlIO $ \run ->
    return $ liftIO $ addMVarFinalizer mvar (run f >> return ())

-- modifyMVar :: MVar a
--            -> (a -> ErrorT MyError IO (a, b))
--            -> ErrorT MyError IO b
-- modifyMVar m io =
--   Control.Exception.Control.mask $ \restore -> do
--     a      <- liftIO $ takeMVar m
--     (a',b) <- restore (io a) `onExceptionError` liftIO (putMVar m a)
--     liftIO $ putMVar m a'
--     return b

------- type
class MonadTrans t where
  lift :: Monad m => m a -> t m a

type Run t = forall n o b. (Monad n, Monad o, Monad (t o)) => t n b -> n (t o b)

class MonadTrans t => MonadTransControl t where
  liftControl :: Monad m => (Run t -> m a) -> t m a

control :: (Monad m, Monad (t m), MonadTransControl t)
        => (Run t -> m (t m a)) -> t m a
control f = join $ liftControl f

type RunInBase m base = forall b. m b -> base (m b)

class MonadIO m => MonadControlIO m where
    liftControlIO :: (RunInBase m IO -> IO a) -> m a

controlIO :: MonadControlIO m => (RunInBase m IO -> IO (m a)) -> m a
controlIO f = join $ liftControlIO f

------- ErrorT instance
newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}

instance Functor m => Functor (ErrorT MyError m) where 
  fmap :: (a -> b) -> ErrorT MyError m a -> ErrorT MyError m b
  fmap f (ErrorT ma) = ErrorT $ fmap (either Left (Right . f)) ma

instance Applicative m => Applicative (ErrorT MyError m) where
  pure :: a -> ErrorT MyError m a
  pure a = ErrorT $ pure $ Right a
  (<*>) :: (ErrorT MyError m) (a -> b) -> ErrorT MyError m a -> ErrorT MyError m b
  (ErrorT mf) <*> (ErrorT ma) = ErrorT $ ((<*>) $ fmap p ma) mf
    where p (Left e) f = Left e
          p (Right a) (Left f) = Left f
          p (Right a) (Right f) = Right $ f a

instance Monad m => Monad (ErrorT MyError m) where
   (>>=) :: ErrorT MyError m a -> (a -> ErrorT MyError m b) -> ErrorT MyError m b
   (ErrorT ma) >>= f = ErrorT $ do
     a <- ma
     case a of
      Left e -> return $ Left e
      Right a -> do
        r' <- r
        return r'
          where r = runErrorT $ f a

instance MonadTrans (ErrorT e) where
  lift :: Monad m => m a -> ErrorT e m a
  lift m = ErrorT $ do
        a <- m
        return (Right a)

unliftErrorT :: Monad m => ErrorT e m a -> m a
unliftErrorT (ErrorT e) = do
  a <- e
  case a of 
    Right p -> return p
    Left q -> undefined

instance MonadTransControl (ErrorT e) where
   liftControl :: Monad m => (Run (ErrorT e) -> m a) -> ErrorT e m a
   -- Run (ErrorT e) :: ErrorT e n b -> n (ErrorT e o b)
   liftControl f = ErrorT $ do
     a <- f (\(ErrorT nb) -> do
       b <- nb
       return $ ErrorT $ return b
        )
     return $ Right a

instance MonadIO m => MonadIO (ErrorT MyError m) where
  liftIO :: IO a -> ErrorT MyError m a
  liftIO m = ErrorT $ liftIO $ do
    a <- m
    return $ Right a

-- instance MonadIO m => MonadControlIO (ErrorT MyError m) where
--   liftControlIO :: (RunInBase (ErrorT MyError m) IO -> IO a) -> ErrorT MyError m a
--   liftControlIO f = liftIO $ f g
--     where g (ErrorT me) = do
--             return $ ErrorT me

instance MonadControlIO (ErrorT MyError IO) where
  liftControlIO :: (RunInBase (ErrorT MyError IO) IO -> IO a) -> ErrorT MyError IO a
  liftControlIO f = liftIO $ f g
    where g (ErrorT me) = do
            e <- me
            return $ ErrorT $ return e

------- ReaderT instance
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where 
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f = \(ReaderT g) -> ReaderT $ fmap f . g

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT $ const $ pure a
  (<*>) :: (ReaderT r m) (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT f) <*> (ReaderT g) = ReaderT $ (\r -> f r <*> g r)

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT f) >>= g = ReaderT $ (\r -> do
    a <- f r
    (runReaderT $ g a) r
    )

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift = ReaderT . const

unliftReaderT :: Monad m => ReaderT r m a -> m a
unliftReaderT (ReaderT f) = f undefined

-- undefined is used
instance MonadTransControl (ReaderT r) where
   liftControl :: Monad m => (Run (ReaderT r) -> m a) -> ReaderT r m a
   -- Run (ReaderT r) :: ReaderT r n b -> n (ReaderT r o b)
   liftControl f = lift $ f g 
     where g (ReaderT h) = do
            b <- h undefined
            return $ ReaderT $ \r' -> return b

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO m = ReaderT $ liftIO . const m

instance MonadIO m => MonadControlIO (ReaderT r m) where
  liftControlIO :: (RunInBase (ReaderT r m) IO -> IO a) -> ReaderT r m a
  -- RunInBase (ReaderT r m) IO :: forall b. ReaderT r m b -> IO (ReaderT r m b)
  liftControlIO f = liftIO $ f return 

------- WriterT instance
newtype WriterT w m a = WriterT {runWriterT :: m (a, w)}

instance Functor m => Functor (WriterT w m) where 
  fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
  fmap f (WriterT m) = WriterT $ fmap (first f) m

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure :: a -> WriterT w m a
  pure a = WriterT $ pure (a, mempty)
  (<*>) :: (WriterT w m) (a -> b) -> WriterT w m a -> WriterT w m b
  (WriterT mf) <*> (WriterT ma) = WriterT $ p <$> mf <*> ma
    where p = uncurry bimap . bimap ($) (<>)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  (WriterT ma) >>= f = WriterT $ do
    (a, w1) <- ma
    (b, w2) <- runWriterT $ f a  
    return (b, w1 <> w2)
  -- the following definition do not satisfy monad rule 2.
  -- (WriterT m) >>= f = WriterT $ do
  --   (a, _w) <- m
  --   runWriterT $ f a

instance Monoid w => MonadTrans (WriterT w) where
  lift :: Monad m => m a -> WriterT w m a
  lift m = WriterT $ do
    a <- m
    return (a, mempty)

unliftWriteT :: Monad m => WriterT w m a -> m a
unliftWriteT (WriterT m) = do
  a <- m
  return $ fst a

instance Monoid w => MonadTransControl (WriterT w) where
  liftControl :: Monad m => (Run (WriterT w) -> m a) -> WriterT w m a
  -- Run (WriterT w) :: WriterT w n b -> n (WriterT w o b)
  liftControl f = lift $ f g
    where g (WriterT m) = do
            aw <- m
            return $ WriterT $ return aw

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
  liftIO :: IO a -> WriterT w m a
  liftIO m = WriterT $ liftIO $ do
    a <- m
    return (a, mempty)

instance (Monoid w, MonadIO m) => MonadControlIO (WriterT w m) where
  liftControlIO :: (RunInBase (WriterT w m) IO -> IO a) -> WriterT w m a
  -- RunInBase (WriterT w m) IO :: forall b. WriterT w m b -> IO (WriterT w m b)
  liftControlIO f = WriterT $ liftIO $ do
    a <- f return 
    return (a, mempty)

------- StateT instance
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where 
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT g) = StateT $ \s -> (fmap $ f *** id) $ g s

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT f) <*> (StateT g) = StateT $ \s -> do
    (h, _t) <- f s
    (a, _u) <- g s
    return $ (h a, s)

instance Monad m => Monad (StateT s m) where
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT f) >>= g = StateT $ \s -> do
    (a, _t) <- f s
    (runStateT $ g a) s

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

unliftStateT :: Monad m => StateT s m a -> m a
unliftStateT (StateT f) = do
  a <- f undefined
  return $ fst a

-- undefined is useds
instance MonadTransControl (StateT s) where
  liftControl :: Monad m => (Run (StateT s) -> m a) -> StateT s m a
  -- Run (StateT s) :: StateT s n b -> n (StateT s o b)
  liftControl f = lift $ f g
    where g (StateT h) = return $ StateT $ \s -> undefined $ h s

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO m = StateT $ \s -> liftIO $ do
    a <- m
    return (a, s)

instance MonadIO m => MonadControlIO (StateT s m) where
  liftControlIO :: (RunInBase (StateT s m) IO -> IO a) -> StateT s m a
  -- RunInBase (StateT s m) IO :: forall b. SytateT s m b -> IO (StateT s m b)
  liftControlIO f = StateT $ \s -> liftIO $ do
    a <- f return
    return (a, s)

main :: IO ()
main = do
  err1 <- runErrorT useMyFileError1
  print $ "err1 is " <> show err1
  --undefined is used in useMyFileError2
  -- err2 <- useMyFileError2
  -- let err2' = runIdentity $ runErrorT err2
  -- print $ "err2 is " <> show err2'
  --code of err3
  a <- runErrorT useMyFileError3
  e <- case a of
    Left b -> return $ Left b
    Right c -> do
      d <- runErrorT c
      return d
  print $ "err3 is " <> show e
  err4 <- runErrorT useMyFileError4
  print $ "err4 is " <> show err4
  err5 <- runErrorT useMyFileError5
  print $ "err5 is " <> show err5
  err6 <- runErrorT useMyFileError6
  print $ "err6 is " <> show err6
  err7 <- runErrorT useMyFileError7
  print $ "err7 is " <> show err7
  exceptionErr <- runErrorT $ onExceptionError (ErrorT $ return (Right "a")) (ErrorT $ return (Left MyError))
  print $ "exceptionErr is " <> show exceptionErr
  mVar <- newMVar 1
  addMVarFinalizerErr <- runErrorT $ addMVarFinalizerError mVar (ErrorT $ return (Left MyError))
  return ()