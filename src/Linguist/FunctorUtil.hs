{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Linguist.FunctorUtil
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
  , PatF(..)
  , Pat(..)
  , closed
  , identity
  , compose
  , composeIdentity
  , free
  , freeComposeIdentity
  , type (~>)
  , type (.~>)
  , type (~>.)
  , imapDefault
  , iFoldMapDefault
  , HFree(..)
  , _I
  , _K
  , _K_I

  , IxFunctor(..)
  , IxFoldable(..)
  , IxTraversable(..)
  ) where

import           Control.Lens
  (Prism', review, prism', iso, Iso', Identity(..), view, _Wrapped, preview)
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Const (Const(..))
import Data.Functor.Foldable (Fix(Fix), unfix)
import Data.Text
import Data.Functor.Foldable (Base)

import Generics.MultiRec.Base (I(I, unI), K(K, unK), unD)
import qualified Generics.MultiRec.Base as MR

infixr 5 ~>
infixr 5 .~>
infixr 5 ~>.

type f ~>  g = forall i. f i -> g i  -- Natural transformation
type f .~> g = forall i. f -> g i   -- Constant on the left
type f ~>. g = forall i. f i -> g   -- Constant on the right

type f :.: g = Compose f g

data (f :+: g) a
  = InL (f a)
  | InR (g a)

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

pattern FreeIn :: f :<: g => f (Free g a) -> Free g a
pattern FreeIn x = Free (In x)

pattern FixIn :: f :<: g => f (Fix g) -> Fix g
pattern FixIn x = Fix (In x)

data PatF f
  = PatVarF !(Maybe Text)
  | BindingPatF ![Text] !f

data Pat = Pat !Text
type instance Base Pat = PatF

-- We can always view a value (Fix f) as a term (Free f Text), but a term is
-- only convertible to a value if it doesn't hold any variables (Pure).
closed :: Traversable f => Prism' (Free f Text) (Fix f)
closed = prism' rtl ltr where
  rtl = Free . fmap rtl . unfix

  ltr = \case
    Pure _  -> Nothing
    Free tm -> Fix <$> traverse ltr tm

identity :: Iso' (Identity a) a
identity = _Wrapped -- iso runIdentity Identity

_I :: Iso' (I xi r ix) (r xi)
_I = iso unI I

compose :: Iso' ((f :.: g) a) (f (g a))
compose = _Wrapped -- iso getCompose Compose

_K :: Iso' (K a r ix) a
_K = iso unK K

composeIdentity :: Functor f => Iso' ((f :.: Identity) a) (f a)
composeIdentity = iso (fmap runIdentity . getCompose) (Compose . fmap Identity)

_K_I :: Functor f => Iso' ((f MR.:.: I ix) a ix) (f (a ix))
_K_I = iso (fmap unI . unD) (MR.D . fmap I)

data HFree f a ix
  = HPure a
  | HFree (f (HFree f a) ix)

free :: Functor f
  => (forall a. Iso'      ((f :.: Identity) a)      (f a))
  -> (forall a. Iso' (Free (f :.: Identity) a) (Free f a))
free i = iso (hoistFree (view i)) (hoistFree (review i))

freeComposeIdentity
  :: Functor f => Iso' (Free (f :.: Identity) Text) (Free f Text)
freeComposeIdentity = free composeIdentity

-- hfree :: Functor f
--   => (forall (a :: * -> *). Iso'       ((f MR.:.: I ix)  a     ix)       (f (a ix)))
--   -> (forall (a :: * -> *). Iso' (HFree (f MR.:.: I ix) (a ix) ix) (Free  f (a ix)))
-- hfree i = iso (hoistHfree (view i)) (hoistHfree (review i))

-- hoistHfree :: IxFunctor g => (f a ~> g a) -> (HFree f b ~> HFree g b)
-- hoistHfree _ (Pure a)  = Pure a
-- hoistHfree f (Free as) = Free (hoistHfree f <$> f as)

-- hfree_K_I
--   :: Functor f => Iso' (HFree (f MR.:.: I ix) Text ix) (Free f Text)
-- hfree_K_I = hfree _K_I

class IxFunctor (f :: (i -> *) -> (o -> *)) where
  imap :: (a ~> b) -> (f a ~> f b)

class IxFunctor t => IxTraversable t where
  -- the type would be "(a ~> (f . b)) -> (t a ~> (f . t b))" if we had the composition
  -- function on type level.
  itraverse :: Applicative f => (forall i. a i -> f (b i)) -> (forall i. t a i -> f (t b i))

class IxFoldable t where
  iFoldMap :: Monoid m => (a ~>. m) -> (t a ~>. m)

imapDefault :: IxTraversable t => (a ~> b) -> (t a ~> t b)
imapDefault f = runIdentity . itraverse (Identity . f)

iFoldMapDefault :: (IxTraversable t, Monoid m) => (a ~>. m) -> (t a ~>. m)
iFoldMapDefault f = getConst . itraverse (Const . f)

-- | Subtyping relation from "Data types a la carte".
--
-- This can be read as "subtype", where we can always 'inject' the subtype into
-- its supertype and sometimes 'project' the supertype down.
class sub :<: sup where
  subtype :: Prism' (sup a) (sub a)

-- inject  :: sub :<: sup => sub a -> sup a
-- inject = review subtype

-- project :: sub :<: sup => sup a -> Maybe (sub a)
-- project = preview subtype

instance f :<: (f :+: g) where
  subtype = _InL

instance g :<: (f :+: g) where
  subtype = _InR

pattern In :: sub :<: sup => sub a -> sup a
pattern In a <- (preview subtype -> Just a) where
  In a = review subtype a
