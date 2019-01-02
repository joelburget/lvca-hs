{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Lvca.FunctorUtil
  ( module Control.Monad.Free
  , module Data.Functor.Foldable
  , module Data.Functor.Const
  , module Data.Functor.Compose
  , module Data.Functor.Classes
  , type (:.:)
  , (:+:)(..)
  , _InL
  , _InR
  , (:<:)(..)
  , pattern In
  , pattern FreeIn
  , pattern FixIn
  , pattern FreeL
  , pattern FreeR
  , pattern FixL
  , pattern FixR

  , Matchable(matchWith, match)
  , Bimatchable(bimatchWith, bimatch)
  ) where

import Control.Lens (Prism', review, prism', preview, Bifunctor)
import Control.Monad.Free
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Const (Const(..))
import Data.Functor.Foldable (Fix(Fix), unfix)

type f :.: g = Compose f g

-- compdata uses precedence 6
-- multirec precendence 5
infixr 6 :+:

data (f :+: g) a
  = InL (f a)
  | InR (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (InL a) = InL (fmap f a)
  fmap f (InR a) = InR (fmap f a)

instance (Show1 f, Show1 g) => Show1 (f :+: g) where
  liftShowsPrec shw shwlist p expf = showParen (p > 10) $ case expf of
    InL a -> showString "InL " . liftShowsPrec shw shwlist 11 a
    InR b -> showString "InR " . liftShowsPrec shw shwlist 11 b

instance (Eq1 f, Eq1 g) => Eq1 (f :+: g) where
  liftEq eq (InL x) (InL y) = liftEq eq x y
  liftEq eq (InR x) (InR y) = liftEq eq x y
  liftEq _  _       _       = False

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap f (InL x) = foldMap f x
  foldMap f (InR x) = foldMap f x

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse f (InL x) = InL <$> traverse f x
  traverse f (InR x) = InR <$> traverse f x

_InL :: Prism' ((f :+: g) a) (f a)
_InL = prism' InL $ \case
  InL fa -> Just fa
  _      -> Nothing

_InR :: Prism' ((f :+: g) a) (g a)
_InR = prism' InR $ \case
  InR ga -> Just ga
  _      -> Nothing

pattern FreeL :: f (Free (f :+: g) a) -> Free (f :+: g) a
pattern FreeL x = Free (InL x)

pattern FreeR :: g (Free (f :+: g) a) -> Free (f :+: g) a
pattern FreeR x = Free (InR x)

pattern FixL :: f (Fix (f :+: g)) -> Fix (f :+: g)
pattern FixL  x = Fix  (InL x)

pattern FixR :: g (Fix (f :+: g)) -> Fix (f :+: g)
pattern FixR  x = Fix  (InR x)

pattern In :: sub :<: sup => sub a -> sup a
pattern In a <- (preview subtype -> Just a) where
  In a = review subtype a

pattern FreeIn :: f :<: g => f (Free g a) -> Free g a
pattern FreeIn x = Free (In x)

pattern FixIn :: f :<: g => f (Fix g) -> Fix g
pattern FixIn x = Fix (In x)

-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class sub :<: sup where
  subtype :: Prism' (sup a) (sub a)

instance f :<: f where
  subtype = id

instance f ~ f' => f :<: (f' :+: g) where
  subtype = _InL

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g)
  => f :<: (h :+: g) where
  subtype = _InR . subtype

-- TODO: this looks like
-- http://hackage.haskell.org/package/matchable-0.1.1.1/docs/Data-Matchable.html
class Functor f => Matchable f where
  match :: f a -> f b -> Maybe (f (a, b))
  match = matchWith (fmap Just . (,))

  matchWith :: (a -> b -> Maybe c) -> f a -> f b -> Maybe (f c)

  default matchWith
    :: Traversable f
    => (a -> b -> Maybe c) -> f a -> f b -> Maybe (f c)
  matchWith f fa fb = do
    fafb <- match fa fb
    traverse (uncurry f) fafb

  {-# MINIMAL matchWith #-}

instance (Matchable f, Matchable g) => Matchable (f :+: g) where
  matchWith f (InL x) (InL y) = InL <$> matchWith f x y
  matchWith f (InR x) (InR y) = InR <$> matchWith f x y
  matchWith _ _       _       = Nothing

class Bifunctor f => Bimatchable f where
  bimatch :: f a x -> f b y -> Maybe (f (a, b) (x, y))
  bimatch = bimatchWith (fmap Just . (,)) (fmap Just . (,))

  bimatchWith
    :: (a -> b -> Maybe c) -> (x -> y -> Maybe z)
    -> f a x -> f b y -> Maybe (f c z)

  default bimatchWith
    :: Bitraversable f
    => (a -> b -> Maybe c) -> (x -> y -> Maybe z)
    -> f a x -> f b y -> Maybe (f c z)
  bimatchWith f g x y = do
    xy <- bimatch x y
    bitraverse (uncurry f) (uncurry g) xy

  {-# MINIMAL bimatchWith #-}
