{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Linguist.Languages.MachineModel
  ( MachineF(..)
  , machineP

  , mkDenotationChart

  -- * Evaluation
  , StackFrame(..)
  , findBinding
  , frameVals
  , StateStep(..)
  , Focus(..)
  ) where

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Control.Lens
  (Prism', _1, _2, (%~), preview, review, prism', (&), _Left)
import Data.Text (Text)

import Linguist.Types                      hiding (patP, termP)
import Linguist.FunctorUtil


data MachineF a
  = Lam
    { _lamBody :: !a
    }
  | App
    { _bodyTerm :: !a
    , _argTerm  :: !a
    }
  | PrimApp
    { _primName    :: !Text
    , _primAppArgs :: ![a]
    }
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq   a) => Eq   (MachineF a)
deriving instance (Show a) => Show (MachineF a)

instance Show1 MachineF where
  liftShowsPrec showsf showListf p tm = showParen (p > 10) $ case tm of
    Lam body ->
        ss "Lam "
      . showsf 11 body
    App body arg ->
        ss "App "
      . showsf 11 body
      . ss " "
      . showsf 11 arg
    PrimApp name args ->
        ss "PrimApp "
      . showsPrec 11 name
      . ss " "
      . showListf args
    where ss = showString

instance Eq1 MachineF where
  liftEq eq (Lam     a1   ) (Lam     a2   ) = eq a1 a2
  liftEq eq (App     a1 b1) (App     a2 b2) = eq a1 a2 && eq b1 b2
  liftEq eq (PrimApp a1 b1) (PrimApp a2 b2) = a1 == a2 && and (zipWith eq b1 b2)
  liftEq _  _               _               = False

machineP
  :: forall f a.
     Prism' (Term (Either Text a))           (Fix f)
  -> Prism' (Term (Either Text a)) (MachineF (Fix f))
machineP p = prism' rtl ltr where
  textP :: Prism' (Term (Either Text a)) Text
  textP = _PrimValue . _Left

  rtl :: MachineF (Fix f) -> Term (Either Text a)
  rtl = \case
    Lam body -> Term "Lam"
      [ review p     body
      ]
    App body arg -> Term "App"
      [ review p body
      , review p arg
      ]
    PrimApp name args -> Term "PrimApp"
      [ review textP     name
      , review (listP p) args
      ]

  ltr :: Term (Either Text a) -> Maybe (MachineF (Fix f))
  ltr = \case
    Term "Lam" [body] -> Lam
      <$> preview p     body
    Term "App" [body, arg] -> App
      <$> preview p body
      <*> preview p arg
    Term "PrimApp" [name, args] -> PrimApp
      <$> preview textP     name
      <*> preview (listP p) args
    _ -> Nothing

mkDenotationChart
  :: Prism' (Pattern a) (Fix (PatF  :+: f))
  -> Prism' (Term    b) (Fix (TermF :+: MeaningOfF :+: g))
  -> DenotationChart' f g
  -> DenotationChart  a b
mkDenotationChart patP termP (DenotationChart' rules) = DenotationChart $
  rules & traverse . _1 %~ review patP
        & traverse . _2 %~ review termP

-- TODO: find a better place to put this
listP :: Prism' (Term a) b -> Prism' (Term a) [b]
listP p = prism' rtl ltr where
  rtl = \case
    []   -> Term "Nil"  []
    x:xs -> Term "Cons" [ review p x , rtl xs ]

  ltr = \case
    Term "Nil"  []      -> Just []
    Term "Cons" [x, xs] -> (:) <$> preview p x <*> ltr xs
    _ -> Nothing

-- evaluation internals

data StackFrame a
  = EvalFrame    !Text !(Term a)
  | BindingFrame !Text !(Term a)

findBinding :: [StackFrame a] -> Text -> Maybe (Term a)
findBinding [] _ = Nothing
findBinding (BindingFrame bname tm : stack) name
  | bname == name = Just tm
  | otherwise = findBinding stack name
findBinding (_ : stack) name = findBinding stack name

frameVals :: [StackFrame a] -> Map Text (Term a)
frameVals = foldl
  (\sorts -> \case
    BindingFrame name tm -> Map.insert name tm sorts
    _                    -> sorts)
  Map.empty

data StateStep a = StateStep
  { _stepFrames :: ![StackFrame a]
  -- | Either descending into term or ascending with value
  , _stepFocus  :: !(Focus a)
  }

  | Errored !Text
  | Done !(Term a)

data Focus a
  = Descending !(Term a)
  | Ascending  !(Term a)
  deriving Show
