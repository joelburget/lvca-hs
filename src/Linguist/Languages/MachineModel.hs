{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE PolyKinds     #-}
module Linguist.Languages.MachineModel
  ( MeaningF(..)
  , meaningPatternVar
  , (//)

  , meaningTermP
  , mkDenotationChart
  , patTermP

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
  (Prism', _1, _2, (%~), preview, review, prism', (&))
import Data.Text (Text)

import Linguist.Types
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

meaningPatternVar :: a -> Term a
meaningPatternVar name = Term "MeaningPatternVar" [ PrimValue name ]

(//) :: Text -> Text -> Term Text -> Term Text
(//) to from term = Term "Renaming" [PrimValue from, PrimValue to, term]

-- The core of this function is the call to meaningTermP, to which we need to
-- pass a prism that it can use on its children. That prism is this one, but
-- generalized.
meaningTermP :: forall a f.
     Prism' a Text
  -> Prism' (Term a) (f (Free (MeaningF :+: f) Text))
  -> Prism' (Term a)    (Free (MeaningF :+: f) Text)
meaningTermP textP fP = prism' rtl ltr where

  primValP :: Prism' (Term a) Text
  primValP = _PrimValue . textP

  subP :: Prism' (Term a) (Free (MeaningF :+: f) Text)
  subP = meaningTermP textP fP

  rtl :: Free (MeaningF :+: f) Text -> Term a
  rtl = \case
    Pure name -> Var name
    Free (InL f) -> case f of
      PrimApp name args -> Term "PrimApp"
        [ review primValP name
        , review (listP subP) args
        ]
      -- Bind from name to -> Term "Bind"
      --   [ review subP from
      --   , review primValP name
      --   , review subP to
      --   ]
      Eval from name to -> Term "Eval"
        [ review subP from
        , Binding [name] (review subP to)
        ]
      Renaming old new term -> Term "Renaming"
        [ review primValP old
        , review primValP new
        , review subP term
        ]
      Value meaning -> Term "Value" [ review subP meaning ]
      MeaningPatternVar name -> Term "MeaningPatternVar" [ PrimValue $ review textP name ]
    Free (InR f) -> review fP f

  ltr :: Term a -> Maybe (Free (MeaningF :+: f) Text)
  ltr = \case
    Var name -> Just $ Pure name
    Term "PrimApp" [name, args] -> fmap (Free . InL) $ PrimApp
      <$> preview primValP name
      <*> preview (listP subP) args
    -- Term "Bind" [from, name, to] -> fmap (Free . InL) $ Bind
    --   <$> preview subP from
    --   <*> preview primValP name
    --   <*> preview subP to
    Term "Eval" [from, Binding [name] to] -> fmap (Free . InL) $ Eval
      <$> preview subP from
      <*> pure name
      <*> preview subP to
    Term "Renaming" [old, new, term] -> fmap (Free . InL) $ Renaming
      <$> preview primValP old
      <*> preview primValP new
      <*> preview subP term
    Term "Value" [ meaning ] -> fmap (Free . InL) $ Value
      <$> preview subP meaning
    Term "MeaningPatternVar" [ PrimValue name ] -> fmap (Free . InL) $
      MeaningPatternVar <$> preview textP name
    other -> fmap (Free . InR) $ preview fP other

patTermP
  :: forall f a.
     Prism' (Pattern a) (f (Fix (PatF :+: f)))
  -> Prism' (Pattern a)    (Fix (PatF :+: f))
patTermP p = prism' rtl ltr where
  rtl :: Fix (PatF :+: f) -> Pattern a
  rtl (Fix f) = case f of
    InL (PatVarF mname)         -> PatternVar mname
    InL (BindingPatF names pat) -> BindingPattern names (rtl pat)
    InR f'                      -> review p f'

  ltr :: Pattern a -> Maybe (Fix (PatF :+: f))
  ltr = \case
    PatternVar mname -> Just $ Fix $ InL $ PatVarF mname
    BindingPattern names pat -> Fix . InL . BindingPatF names <$> ltr pat
    other -> Fix . InR <$> preview p other

mkDenotationChart
  :: (f' ~ (PatF :+: f), g' ~ (MeaningF :+: g))
  => Prism' b Text
  -> Prism' (Pattern a) (f (Fix f'))
  -> Prism' (Term b)    (g (Free g' Text))
  -> DenotationChart' f' g'
  -> DenotationChart a b
mkDenotationChart textP patP termP (DenotationChart' rules) = DenotationChart $
  rules & traverse . _1 %~ review (patTermP patP)
        & traverse . _2 %~ review (meaningTermP textP termP)

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
