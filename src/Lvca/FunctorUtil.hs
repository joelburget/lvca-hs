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
  , module Data.Bimatchable
  , module Data.Matchable
  , type (:.:)
  , (:+:)(..)
  , _InL
  , _InR
  , _Fix
  , (:<:)(..)
  , pattern In
  , pattern FreeIn
  , pattern FixIn
  , pattern FreeL
  , pattern FreeR
  , pattern FixL
  , pattern FixR
  ) where

import Control.Lens (Iso', Prism', iso, review, prism', preview)
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Const (Const(..))
import Data.Functor.Foldable (Fix(Fix), unfix)
import Data.Bimatchable
import Data.Matchable

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

_Fix :: Iso' (Fix f) (f (Fix f))
_Fix = iso unfix Fix

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
