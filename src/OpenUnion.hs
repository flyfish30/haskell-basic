{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE KindSignatures, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenUnion where

import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Unsafe.Coerce

-- The Union2 is isomorhic with Either, Inj0 as Left, Inj1 as Right
data Union3 a b c = Inj0 a
                  | Inj1 b
                  | Inj2 c
                  deriving (Show)

inj0 :: a -> Union3 a b c
inj0 = Inj0

inj1 :: b -> Union3 a b c
inj1 = Inj1

inj2 :: c -> Union3 a b c
inj2 = Inj2

prj0 :: Union3 a b c -> a
prj0 (Inj0 x) = unsafeCoerce x

prj1 :: Union3 a b c -> b
prj1 (Inj1 x) = unsafeCoerce x

prj2 :: Union3 a b c -> c
prj2 (Inj2 x) = unsafeCoerce x

type family ULen (r :: [*]) :: Nat where
    ULen '[] = 0
    ULen (t ': ts) = 1 + ULen ts

type family ElemIdx t (r :: [*]) :: Nat where
    ElemIdx t '[]  = TypeError ('Text "Union No type: " ':<>: 'ShowType t)
    ElemIdx t (t  ': rs) = 0
    ElemIdx t (t' ': rs) = 1 + ElemIdx t rs

-- data Uion (r :: [*]) = Union !Word t
data Union (r :: [*]) where
  Union :: !Word -> t -> Union r

unsafeInj :: Word -> t -> Union r
unsafeInj = Union
{-# INLINE unsafeInj #-}

unsafePrj :: Word -> Union r -> Maybe t
unsafePrj n (Union idx a)
  | n == idx  = Just (unsafeCoerce a)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

injn :: forall n t r. (KnownNat n, n ~ ElemIdx t r) => Proxy n -> t -> Union r
injn p a = unsafeInj (fromIntegral $ natVal p) a

prjn :: forall n t r. (KnownNat n, n ~ ElemIdx t r) => Proxy n -> Union r -> Maybe t
prjn p = unsafePrj $ fromIntegral (natVal p)

inj :: forall n t r. (KnownNat n, n ~ ElemIdx t r) => t -> Union r
inj = unsafeInj (fromIntegral $ natVal (Proxy @n))

prj :: forall n t r. (KnownNat n, n ~ ElemIdx t r) => Union r -> Maybe t
prj = unsafePrj (fromIntegral $ natVal (Proxy @n))

decomp :: Union (t ': r) -> Either t (Union r)
decomp (Union 0 a) = Left $ unsafeCoerce a
decomp (Union n a) = Right (Union (n - 1) a)

weaken :: Union r -> Union (t ': r)
weaken (Union n a) = Union (n + 1) a

match :: (forall t. t -> b) -> Union r -> b
match f (Union _ a) = f a
