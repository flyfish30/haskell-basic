{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Data.Char

-- | definition for Yoneda
newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

instance Functor (Yoneda f) where
  fmap f y = Yoneda (\bc -> runYoneda y (bc . f))

liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda fa = Yoneda (\f -> fmap f fa)

lowerYoneda :: Functor f => Yoneda f a -> f a
lowerYoneda y = runYoneda y id

sampleList = [1..10000]
mapListYoneda = lowerYoneda . fmap (*8) . fmap (+20) . liftYoneda $ sampleList
mapList = map (*8) . fmap (+20) $ sampleList

-- | definition for Coyoneda
data Coyoneda f a = forall b. Coyoneda (b -> a) (f b)

instance Functor (Coyoneda f) where
  fmap f (Coyoneda ba fb) = Coyoneda (f . ba) fb

liftCoyoneda :: f a -> Coyoneda f a
liftCoyoneda fa = Coyoneda id fa

lowerCoyoneda :: Functor f => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda ba fb) = fmap ba fb

ntCoyo :: (forall a. f a -> g a) -> Coyoneda f a -> Coyoneda g a
ntCoyo f (Coyoneda ba fb) = Coyoneda ba (f fb)

data Stroll a where
  Forward :: Int -> Stroll Int
  Backward :: Int-> Stroll Int
  Stop :: Stroll a

strollSample = Backward 20

strollToStepCount :: Stroll a -> Maybe a
strollToStepCount (Forward n)  = Just n
strollToStepCount (Backward n) = Just (-n)
strollToStepCount Stop = Nothing

getStepCount :: Num a => Stroll a -> Maybe a
getStepCount = lowerCoyoneda . ntCoyo strollToStepCount
             . fmap (*3) . fmap (+4) . liftCoyoneda

