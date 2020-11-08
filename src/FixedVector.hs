{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE TypeInType, DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module FixedVector where

import Prelude hiding ((++))
import GHC.TypeLits
import Data.Type.Equality
import Data.Kind
import Data.Proxy
import Data.Coerce
import Data.Finite
import Data.Finite.Internal
import Data.Vector as V
import Control.Comonad

newtype Vec (n::Nat) a = UnsafeMkVec { getVector :: V.Vector a }
                      deriving (Show)

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec vec
  | V.length vec == l = Just (UnsafeMkVec vec)
  | otherwise         = Nothing
  where l = fromIntegral $ natVal (Proxy @n)

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f vec = coerce $ V.map f (getVector vec)

imapVec :: (Finite n -> a -> b) -> Vec n a -> Vec n b
imapVec f vec = coerce $ V.imap (f . Finite . fromIntegral) (getVector vec)

instance (KnownNat n, Semigroup a) => Semigroup (Vec n a) where
  (<>) = zipWithVec (<>)

instance (KnownNat n, Monoid a) => Monoid (Vec n a) where
  mempty = replicateVec mempty
  mappend = zipWithVec mappend

instance Functor (Vec n) where
  fmap = mapVec

instance KnownNat n => Applicative (Vec n) where
  pure = replicateVec
  (<*>)  = zipWithVec ($)

instance KnownNat n => Monad (Vec n) where
  return = replicateVec
  vec >>= f = imapVec (\i x -> f x `indexVec` i) vec

join :: (Vec n (Vec n a)) -> Vec n a
join vvec = imapVec (\i x -> x `indexVec` i) vvec

instance (KnownNat n, n ~ (1 + m)) => Comonad (Vec n) where
  extract = headVec
  extend f vec = coerce $ V.generate l (f . flip rotateLeftVec vec)
    where l = fromIntegral $ natVal (Proxy @n)
  duplicate vec = coerce $ V.generate l (flip rotateLeftVec vec)
    where l = fromIntegral $ natVal (Proxy @n)

headVec :: Vec (1+n) a -> a
headVec vec = V.head $ getVector vec

tailVec :: Vec (1+n) a -> Vec n a
tailVec vec = coerce $ V.tail $ getVector vec

initVec :: Vec (n+1) a -> Vec n a
initVec vec = coerce $ V.init $ getVector vec

lastVec :: Vec (n+1) a -> a
lastVec vec = V.last $ getVector vec

appendVec :: Vec n a -> Vec m a -> Vec (n + m) a
appendVec vec1 vec2 = UnsafeMkVec $ getVector vec1 V.++ getVector vec2

(++) = appendVec

rotateLeftVec :: forall n a. KnownNat n => Int -> Vec n a -> Vec n a
rotateLeftVec nr vec = coerce $ v1 V.++ v2
  where v1 = V.slice nr (l - nr) (getVector vec)
        v2 = V.slice 0 nr (getVector vec)
        l = fromIntegral $ natVal (Proxy @n)

rotateRightVec :: forall n a. KnownNat n => Int -> Vec n a -> Vec n a
rotateRightVec nr vec = coerce $ v2 V.++ v1
  where v1 = V.slice 0 (l - nr) (getVector vec)
        v2 = V.slice (l - nr) nr (getVector vec)
        l = fromIntegral $ natVal (Proxy @n)

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec vec1 vec2 = UnsafeMkVec $ V.zip (getVector vec1) (getVector vec2)

takeVec :: forall n m a. KnownNat n => Vec (n + m) a -> Vec n a
takeVec vec = UnsafeMkVec $ V.take l (getVector vec)
  where l = fromIntegral $ natVal (Proxy @n)

takeVec' :: KnownNat n => Proxy n -> Vec (n + m) a -> Vec n a
takeVec' pn vec = UnsafeMkVec $ V.take l (getVector vec)
  where l = fromIntegral $ natVal pn

splitVec :: forall n m a. KnownNat n => Vec (n + m) a -> (Vec n a, Vec m a)
splitVec vec = (UnsafeMkVec xs, UnsafeMkVec ys)
  where l = fromIntegral $ natVal (Proxy @n)
        (xs, ys) = V.splitAt l (getVector vec)

zipWithVec :: (a -> b -> c) -> Vec n a -> Vec m b -> Vec n1 c
zipWithVec f u v = UnsafeMkVec $ V.zipWith f (getVector u) (getVector v)

replicateVec :: forall n a. KnownNat n => a -> Vec n a
replicateVec a = UnsafeMkVec $ V.replicate l a
  where l = fromIntegral $ natVal (Proxy @n)

indexVec :: Vec n a -> Finite n -> a
indexVec v (Finite i) = (getVector v) `V.unsafeIndex` fromIntegral i

generateVec :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
generateVec g = UnsafeMkVec $ V.generate l (g . fromIntegral)
  where l = fromIntegral $ natVal (Proxy @n)

withVec :: V.Vector a -> (forall n. KnownNat n => Vec n a -> r) -> r
withVec v f = case (someNatVal $ fromIntegral $ V.length v) of
  Just (SomeNat (Proxy :: Proxy m)) -> f (UnsafeMkVec @m v)
  Nothing -> f (UnsafeMkVec @0 v)

exactLength :: forall n m a. (KnownNat n, KnownNat m) => Vec n a -> Maybe (Vec m a)
exactLength vec = case sameNat (Proxy @n) (Proxy @m) of
  Just Refl -> Just vec
  Nothing   -> Nothing

zipSame :: V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
zipSame v1 v2 = withVec v1 $ \(v1' :: Vec n a) ->
                withVec v2 $ \(v2' :: Vec m b) ->
                case exactLength v1' of
                  Just v1Same -> Just $ getVector (zipVec v1Same v2')
                  Nothing     -> Nothing

vec2x3Int :: Maybe (Vec (2*3) Int)
vec2x3Int = mkVec (V.fromList [1..7])
