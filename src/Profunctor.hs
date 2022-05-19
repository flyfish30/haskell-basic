{-# LANGUAGE ExistentialQuantification #-}

import Data.Either
import Control.Applicative

-- Contravariant Functor
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

newtype Predicate a = Predicate { getPredicate :: a -> Bool }
instance Contravariant Predicate where
  contramap g (Predicate p) = Predicate (p . g)

newtype Op a b = Op { getOp :: (b -> a) }
instance Contravariant (Op a) where
  contramap g (Op f) = Op (f . g)

-- Bifunctor Functor
class Bifunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance Bifunctor Either where
  bimap g h = either (Left . g) (Right . h)

instance Bifunctor (,) where
  bimap = (><)
  -- bimap g h (a, b) = (g a, h b)

-- Profunctor Functor
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap g h = lmap g . rmap h

  lmap :: (a -> b) -> p b c -> p a c
  lmap g = dimap g id

  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id

instance Profunctor (->) where
  dimap = (>*<)
  -- dimap g h f = h . f . g

infixr 4 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
f >< g = \(a, c) -> (f a, g c)

-- prodComp == compProd
prodComp = \f f1 g g1 -> (f . f1) >< (g . g1)
compProd = \f f1 g g1 -> (f >< g) . (f1 >< g1)

infixr 4 >*<
(>*<) :: (a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
f >*< g = \h -> g . h . f

-- tprodComp == compTprod
tprodComp = \f f1 g g1 -> (f1 . f) >*< (g . g1)
compTprod = \f f1 g g1 -> (f >*< g) . (f1 >*< g1)

data Day f g a = forall b c. Day (f b) (g c) ((b, c) -> a)
instance (Functor f, Functor g) => Functor (Day f g) where
  fmap f (Day fb gc bca) = Day fb gc (f . bca)

instance (Applicative f, Applicative g) => Applicative (Day f g) where
  pure x = Day (pure ()) (pure ()) (const x)
  (Day fd ge def) <*> (Day fb gc bca) = Day ((,) <$> fd <*> fb) ((,) <$> ge <*> gc)
                                          (\((d, b), (e, c)) -> (def (d, e)) (bca (b, c)))

day :: f b -> g c -> Day f g (b, c)
day fb gc = Day fb gc id

dap :: Applicative f => Day f f a -> f a
dap (Day f1 f2 g) = fmap g (liftA2 (,) f1 f2)

{-
instance Functor ((->) r) where
  fmap = (.)

instance Applicative ((->) r) where
  pure = const
  af <*> f = \x -> af x (f x)
-}

class Functor f => Monoidal f where
  unit :: f ()
  (<.>) :: f a -> f b -> f (a, b)

instance Monoidal [] where
  unit = pure ()
  fa <.> fb = (,) <$> fa <*> fb

-- pure x = fmap (const x) unit
-- ff <*> fx = fmap (uncurry ($)) (ff <.> fx)

newtype Id a = Id { getId :: a}
instance Functor Id where
  fmap f (Id a) = Id (f a)

{-
infixr 0 ~>
type f ~> g = forall x. f x -> g x

infixr 0 :~>, $$
newtype f :~> g = Nat { ($$) :: f ~> g }
-}

