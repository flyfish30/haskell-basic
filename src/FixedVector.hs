{-# LANGUAGE TypeOperators, TypeApplications #-}
{-# LANGUAGE TypeInType, DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module FixedVector where

import Prelude hiding ((++))
import Type.Errors
import GHC.TypeLits
import Unsafe.Coerce
import Data.Typeable
import Data.Int
import Data.Word
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

instance Functor (Vec n) where
  fmap = mapVec

instance KnownNat n => Applicative (Vec n) where
  pure = replicateVec
  (<*>)  = zipWithVec ($)

instance KnownNat n => Monad (Vec n) where
  vec >>= f = imapVec (\i x -> f x `indexVec` i) vec

join :: (Vec n (Vec n a)) -> Vec n a
join vvec = imapVec (\i x -> x `indexVec` i) vvec

instance (KnownNat n, n ~ (1 + m)) => Comonad (Vec n) where
  extract = headVec
  extend f vec = coerce $ V.generate l (f . flip rotateLeftVec vec)
    where l = fromIntegral $ natVal (Proxy @n)
  duplicate vec = coerce $ V.generate l (flip rotateLeftVec vec)
    where l = fromIntegral $ natVal (Proxy @n)

instance (KnownNat n, Num a) => Num (Vec n a) where
  v1 + v2 = (+) <$> v1 <*> v2
  v1 - v2 = (-) <$> v1 <*> v2
  v1 * v2 = (*) <$> v1 <*> v2
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = replicateVec . fromInteger

instance (KnownNat n, Fractional a) => Fractional (Vec n a) where
  v1 / v2 = (/) <$> v1 <*> v2
  recip = fmap recip
  fromRational = replicateVec . fromRational

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

-- | zip two same length fixed vectory
zipWithVec :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWithVec f u v = UnsafeMkVec $ V.zipWith f (getVector u) (getVector v)

-- | zip two difference length fixed vectory
zipWithVecUnalign :: (a -> b -> c) -> Vec n a -> Vec m b -> Vec n1 c
zipWithVecUnalign f u v = UnsafeMkVec $ V.zipWith f (getVector u) (getVector v)

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

type SimdLane :: Type
data SimdLane where
  SimdLaneN :: Nat -> SimdLane
  deriving (Show, Read, Eq)

type family SlaneToNat (l::SimdLane) :: Nat where
  SlaneToNat (SimdLaneN n) = n

type SimdElem :: Type
data SimdElem where
  SimdIntN   :: Nat -> SimdElem
  SimdUintN  :: Nat -> SimdElem
  SimdFloatN :: Nat -> SimdElem
  deriving (Show, Read, Eq)

type family SelemToType (e::SimdElem) where
  SelemToType (SimdIntN 8)  = Int8
  SelemToType (SimdIntN 16) = Int16
  SelemToType (SimdIntN 32) = Int32
  SelemToType (SimdIntN 64) = Int64

  SelemToType (SimdUintN 8)  = Word8
  SelemToType (SimdUintN 16) = Word16
  SelemToType (SimdUintN 32) = Word32
  SelemToType (SimdUintN 64) = Word64

  SelemToType (SimdFloatN 32) = Float
  SelemToType (SimdFloatN 64) = Double

  SelemToType e = TypeError ('Text "Can not support this SIMD primary type: " :<>: ShowType e)

type family TypeToSelem (a::Type) :: SimdElem where
  TypeToSelem Int8  = (SimdIntN 8)
  TypeToSelem Int16 = (SimdIntN 16)
  TypeToSelem Int32 = (SimdIntN 32)
  TypeToSelem Int64 = (SimdIntN 64)

  TypeToSelem Word8  = (SimdUintN 8)
  TypeToSelem Word16 = (SimdUintN 16)
  TypeToSelem Word32 = (SimdUintN 32)
  TypeToSelem Word64 = (SimdUintN 64)

  TypeToSelem Float  = (SimdFloatN 32)
  TypeToSelem Double = (SimdFloatN 64)

  TypeToSelem a = TypeError ('Text "Can not support the type " :<>: ShowType a :<>:
                             'Text " for SIMD")

newtype SimdVec l e = MkSimdVec { simdVector :: Vec (SlaneToNat l) (SelemToType e) }

mapSimdVec f (MkSimdVec v1) (MkSimdVec v2) = MkSimdVec $ f v1 v2

-- | deriving Show typeclass
deriving instance Show (SimdVec l (SimdIntN 8))
deriving instance Show (SimdVec l (SimdIntN 16))
deriving instance Show (SimdVec l (SimdIntN 32))
deriving instance Show (SimdVec l (SimdIntN 64))

deriving instance Show (SimdVec l (SimdUintN 8))
deriving instance Show (SimdVec l (SimdUintN 16))
deriving instance Show (SimdVec l (SimdUintN 32))
deriving instance Show (SimdVec l (SimdUintN 64))

deriving instance Show (SimdVec l (SimdFloatN 32))
deriving instance Show (SimdVec l (SimdFloatN 64))

-- | deriving Num typeclass
-- deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdIntN e))
instance (KnownNat l, KnownNat n) => Num (SimdVec (SimdLaneN l) (SimdIntN n)) where
  (+) = simdIntAdd
  (-) = simdIntSub
  (*) = simdIntMul
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

castSimdVec :: (KnownNat dl, KnownNat dn)
            => SimdVec (SimdLaneN l) (SimdIntN n)
            -> SimdVec (SimdLaneN dl) (SimdIntN dn)
castSimdVec = unsafeCoerce

isInt8x16NormOp :: (KnownNat l1, KnownNat n1, KnownNat l2, KnownNat n2)
                => SimdVec (SimdLaneN l1) (SimdIntN n1)
                -> SimdVec (SimdLaneN l2) (SimdIntN n2)
                -> Bool
isInt8x16NormOp v1 v2 = typeOf v1 == typeRep (Proxy @(SimdVec (SimdLaneN 16) (SimdIntN 8)))
                      && typeOf v2 == typeRep (Proxy @(SimdVec (SimdLaneN 16) (SimdIntN 8)))

isInt16x8NormOp :: (KnownNat l1, KnownNat n1, KnownNat l2, KnownNat n2)
                => SimdVec (SimdLaneN l1) (SimdIntN n1)
                -> SimdVec (SimdLaneN l2) (SimdIntN n2)
                -> Bool
isInt16x8NormOp v1 v2 = typeOf v1 == typeRep (Proxy @(SimdVec (SimdLaneN 8) (SimdIntN 16)))
                      && typeOf v2 == typeRep (Proxy @(SimdVec (SimdLaneN 8) (SimdIntN 16)))

isInt8x16WideOp :: (KnownNat l1, KnownNat n1, KnownNat l2, KnownNat n2)
                => SimdVec (SimdLaneN l1) (SimdIntN n1)
                -> SimdVec (SimdLaneN l2) (SimdIntN n2)
                -> Bool
isInt8x16WideOp v1 v2 = typeOf v1 == typeRep (Proxy @(SimdVec (SimdLaneN 8) (SimdIntN 16)))
                      && typeOf v2 == typeRep (Proxy @(SimdVec (SimdLaneN 16) (SimdIntN 8)))

simdIntAdd :: (KnownNat l1, KnownNat n1, KnownNat l2, KnownNat n2)
           => SimdVec (SimdLaneN l1) (SimdIntN n1)
           -> SimdVec (SimdLaneN l2) (SimdIntN n2)
           -> SimdVec (SimdLaneN l1) (SimdIntN n1)
simdIntAdd v1 v2
  | isInt8x16NormOp v1 v2 = unsafeCoerce
                          $ simdAddInt8x16 (castSimdVec @16 @8 v1)
                                           (castSimdVec @16 @8 v2)
  | isInt16x8NormOp v1 v2 = unsafeCoerce
                          $ simdAddInt16x8 (castSimdVec @8 @16 v1)
                                           (castSimdVec @8 @16 v2)
  | isInt8x16WideOp v1 v2 = unsafeCoerce
                          $ simdAddwInt8x16 (castSimdVec @8 @16 v1)
                                            (castSimdVec @16 @8 v2)


simdIntSub :: (KnownNat l1, KnownNat n1, KnownNat l2, KnownNat n2)
           => SimdVec (SimdLaneN l1) (SimdIntN n1)
           -> SimdVec (SimdLaneN l2) (SimdIntN n2)
           -> SimdVec (SimdLaneN l1) (SimdIntN n1)
simdIntSub v1 v2
  | isInt8x16NormOp v1 v2 = unsafeCoerce
                          $ simdSubInt8x16 (castSimdVec @16 @8 v1)
                                           (castSimdVec @16 @8 v2)
  | isInt16x8NormOp v1 v2 = unsafeCoerce
                          $ simdSubInt16x8 (castSimdVec @8 @16 v1)
                                           (castSimdVec @8 @16 v2)


simdIntMul :: (KnownNat l1, KnownNat n1, KnownNat l2, KnownNat n2)
           => SimdVec (SimdLaneN l1) (SimdIntN n1)
           -> SimdVec (SimdLaneN l2) (SimdIntN n2)
           -> SimdVec (SimdLaneN l1) (SimdIntN n1)
simdIntMul v1 v2
  | isInt8x16NormOp v1 v2 = unsafeCoerce
                          $ simdMulInt8x16 (castSimdVec @16 @8 v1)
                                           (castSimdVec @16 @8 v2)
  | isInt16x8NormOp v1 v2 = unsafeCoerce
                          $ simdMulInt16x8 (castSimdVec @8 @16 v1)
                                           (castSimdVec @8 @16 v2)

simdAddInt8x16 :: SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
simdAddInt8x16 = mapSimdVec (+)

simdAddInt16x8 :: SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
simdAddInt16x8 = mapSimdVec (+)

simdAddwInt8x16 :: SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
simdAddwInt8x16 = \_ _ -> 42 :: SimdVec (SimdLaneN 8) (SimdIntN 16)


simdSubInt8x16 :: SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
simdSubInt8x16 = mapSimdVec (-)

simdSubInt16x8 :: SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
simdSubInt16x8 = mapSimdVec (-)


simdMulInt8x16 :: SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
               -> SimdVec (SimdLaneN 16) (SimdIntN 8)
simdMulInt8x16 = mapSimdVec (*)

simdMulInt16x8 :: SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
               -> SimdVec (SimdLaneN 8) (SimdIntN 16)
simdMulInt16x8 = mapSimdVec (*)

instance (KnownNat l, KnownNat n) => Num (SimdVec (SimdLaneN l) (SimdUintN n)) where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
{-
deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdUintN 8))
deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdUintN 16))
deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdUintN 32))
deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdUintN 64))
-}

instance (KnownNat l, KnownNat n) => Num (SimdVec (SimdLaneN l) (SimdFloatN n)) where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
{-
deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdFloatN 32))
deriving instance KnownNat l => Num (SimdVec (SimdLaneN l) (SimdFloatN 64))
-}

-- | deriving Fractional typeclass
deriving instance KnownNat l => Fractional (SimdVec (SimdLaneN l) (SimdFloatN 32))
deriving instance KnownNat l => Fractional (SimdVec (SimdLaneN l) (SimdFloatN 64))


vec2x3Int :: Maybe (Vec (2*3) Int)
vec2x3Int = mkVec (V.fromList [1..7])
