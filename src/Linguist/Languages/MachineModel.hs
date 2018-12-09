{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Linguist.Languages.MachineModel
  ( MeaningF(..)
  , meaningPatternVar
  , (//)

  , meaningP
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


data MeaningF a
  = PrimApp
    { _primName    :: !Text
    , _primAppArgs :: ![a]
    }
  -- | Bind
  --   { _bindFrom :: !a
  --   , _patName  :: !Text
  --   , _bindTo   :: !a
  --   }
  | Eval
    { _evalFrom :: !a
    , _patName  :: !Text
    , _evalTo   :: !a
    }
  | Renaming
    { _oldName :: !Text
    , _newName :: !Text
    , _inTerm  :: !a
    }
  | Value !a
  | MeaningPatternVar !Text
  deriving (Functor, Foldable, Traversable)

deriving instance (Eq   a) => Eq   (MeaningF a)
deriving instance (Show a) => Show (MeaningF a)

instance Show1 MeaningF where
  liftShowsPrec showsf showListf p tm = showParen (p > 10) $ case tm of
    PrimApp name args ->
        ss "PrimApp "
      . showsPrec 11 name
      . ss " "
      . showListf args
    Eval from name to ->
        ss "Eval "
      . showsf 11 from
      . ss " "
      . showsPrec 11 name
      . ss " "
      . showsf 11 to
    Renaming from to tm' ->
        ss "Renaming "
      . showsPrec 11 from
      . ss " "
      . showsPrec 11 to
      . ss " "
      . showsf 11 tm'
    Value tm'              -> ss "Value " . showsf 11 tm'
    MeaningPatternVar name -> ss "MeaningPatternVar " . showsPrec 11 name
    where ss = showString

instance Eq1 MeaningF where
  liftEq eqf (PrimApp name1 args1) (PrimApp name2 args2)
    = name1 == name2 && and (zipWith eqf args1 args2)
  liftEq eqf (Eval from1 name1 to1) (Eval from2 name2 to2)
    = eqf from1 from2 && name1 == name2 && eqf to1 to2
  liftEq eqf (Renaming from1 to1 tm'1) (Renaming from2 to2 tm'2)
    = from1 == from2 && to1 == to2 && eqf tm'1 tm'2
  liftEq eqf (Value v1) (Value v2) = eqf v1 v2
  liftEq _ (MeaningPatternVar v1) (MeaningPatternVar v2) = v1 == v2
  liftEq _ _ _ = False

meaningPatternVar :: Text -> Term (Either Text a)
meaningPatternVar name = Term "MeaningPatternVar" [ PrimValue (Left name) ]

(//) :: Text -> Text -> Term (Either Text a) -> Term (Either Text a)
(//) to from term = Term "Renaming"
  [ PrimValue (Left from)
  , PrimValue (Left to)
  , term
  ]

meaningP
  :: Prism' (Term (Either Text a))           (Fix f)
  -> Prism' (Term (Either Text a)) (MeaningF (Fix f))
meaningP p = prism' rtl ltr where
  primValP :: Prism' (Term (Either Text a)) Text
  primValP = _PrimValue . _Left

  rtl = \case
    PrimApp name args -> Term "PrimApp"
      [ review primValP name
      , review (listP p) args
      ]
    -- Bind from name to -> Term "Bind"
    --   [ review p from
    --   , review primValP name
    --   , review p to
    --   ]
    Eval from name to -> Term "Eval"
      [ review p from
      , Binding [name] (review p to)
      ]
    Renaming old new term -> Term "Renaming"
      [ review primValP old
      , review primValP new
      , review p term
      ]
    Value meaning -> Term "Value" [ review p meaning ]
    MeaningPatternVar name -> Term "MeaningPatternVar"
      [ PrimValue $ review _Left name ]

  ltr = \case
    -- Var name -> Just $ Pure name
    Term "PrimApp" [name, args] -> PrimApp
      <$> preview primValP name
      <*> preview (listP p) args
    -- Term "Bind" [from, name, to] -> Bind
    --   <$> preview p from
    --   <*> preview primValP name
    --   <*> preview p to
    Term "Eval" [from, Binding [name] to] -> Eval
      <$> preview p from
      <*> pure name
      <*> preview p to
    Term "Renaming" [old, new, term] -> Renaming
      <$> preview primValP old
      <*> preview primValP new
      <*> preview p term
    Term "Value" [ meaning ] -> Value <$> preview p meaning
    Term "MeaningPatternVar" [ PrimValue name ] ->
      MeaningPatternVar <$> preview _Left name
    _ -> Nothing

mkDenotationChart
  :: Prism' (Pattern a) (Fix (PatF  :+: f))
  -> Prism' (Term    b) (Fix (TermF :+: g))
  -> DenotationChart' f g
  -> DenotationChart  a b
mkDenotationChart patP termP (DenotationChart' rules) = DenotationChart $
  rules & traverse . _1 %~ review patP
        & traverse . _2 %~ review termP

-- TODO: find a better place to put this
listP :: Prism' (Term a) b -> Prism' (Term a) [b]
listP p = prism' rtl ltr where
  rtl = \case
    []   -> Term "Nil" []
    x:xs -> Term "Cons"
      [ review p x
      , rtl xs
      ]

  ltr = \case
    Term "Nil" []       -> Just []
    Term "Cons" [x, xs] -> (:)
      <$> preview p x
      <*> ltr xs
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
