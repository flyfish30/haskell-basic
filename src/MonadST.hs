{-# LANGUAGE RankNTypes #-}

module MonadST where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

newtype ST s a = ST { unsafeRunST :: a }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  ST f <*> ST a = seq f . seq a . ST $ f a

instance Monad (ST s) where
  return = ST
  ST a >>= f = seq a $ f a

newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = ST . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = ST . unsafePerformIO . readIORef . unSTRef 

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = ST . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = ST . unsafePerformIO $ modifyIORef (unSTRef ref) f

runST :: (forall s. ST s a) -> a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
  ref <- newSTRef "Hello"
  modifySTRef ref (++ " world!")
  readSTRef ref
