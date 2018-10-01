{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE PolyKinds     #-}
module Linguist.Languages.MachineModel
  ( MeaningF(..)

  -- , patName
  , meaningTermP
  -- , meaningOfTermP
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
-- import qualified Control.Lens as Lens
import           Control.Lens
  (Prism', _1, _2, (%~), preview, review, prism', (^?), ix, (&))
import Data.Text (Text)
import           Data.Sequence             (Seq)

import Linguist.Types
import Linguist.FunctorUtil


data MeaningF a
  = PrimApp
    { _primName    :: !Text
    , _primAppArgs :: ![a]
    }
  | Bind
    { _bindFrom :: !a
    , _patName  :: !Text
    , _bindTo   :: !a
    }
  | Eval
    { _evalFrom :: !a
    , _patName  :: !Text
    , _evalTo   :: !a
    }
  | Value !a
  | MeaningVar !Text -- XXX how is this different from Pure?
  deriving (Functor, Foldable, Traversable)

deriving instance (Show k) => Show (MeaningF k)
deriving instance (  Eq k) => Eq   (MeaningF k)

-- newtype MeaningTerm  = MeaningTerm  { viewMeaningTerm  :: Free MeaningF Text }
-- newtype MeaningValue = MeaningValue { viewMeaningValue :: Fix  MeaningF      }

-- The core of this function is the call to meaningOfTermP, to which we need to
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
      Bind from name to -> Term "Bind"
        [ review subP from
        , review primValP name
        , review subP to
        ]
      Eval from name to -> Term "Eval"
        [ review subP from
        , review primValP name
        , review subP to
        ]
      Value meaning -> Term "Value" [ review subP meaning ]
      MeaningVar name -> Term "MeaningVar" [ review primValP name ]
    Free (InR f) -> review fP f

  ltr :: Term a -> Maybe (Free (MeaningF :+: f) Text)
  ltr = \case
    Var name -> Just $ Pure name
    Term "PrimApp" [name, args] -> fmap (Free . InL) $ PrimApp
      <$> preview primValP name
      <*> preview (listP subP) args
    Term "Bind" [from, name, to] -> fmap (Free . InL) $ Bind
      <$> preview subP from
      <*> preview primValP name
      <*> preview subP to
    Term "Eval" [from, name, to] -> fmap (Free . InL) $ Eval
      <$> preview subP from
      <*> preview primValP name
      <*> preview subP to
    Term "Value" [ meaning ] -> fmap (Free . InL) $ Value
      <$> preview subP meaning
    Term "MeaningVar" [ name ] -> fmap (Free . InL) $ MeaningVar
      <$> preview primValP name
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
  -> Prism' (Term b) (g (Free g' Text))
  -> DenotationChart' f' g'
  -> DenotationChart a b
mkDenotationChart textP p1 p2 (DenotationChart' rules) = DenotationChart $
  rules & traverse . _1 %~ review (patTermP p1)
        & traverse . _2 %~ review (meaningTermP textP p2)

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
  = CbvForeignFrame
    { _frameName  :: !Text                     -- ^ name of this term
    , _frameVals  :: !(Seq (Term a))           -- ^ values before
    , _frameTerms :: ![Term a]                 -- ^ subterms after
    , _frameK     :: !(Seq (Term a) -> Term a) -- ^ what to do after
    }
  | CbvFrame
    { _frameName        :: !Text
    , _cbvFrameArgNames :: ![Text]
    , _cbvFrameVals     :: !(Seq (Term a))
    , _cbvFrameTerms    :: ![Term a]
    , _cbvFrameBody     :: !(Term a)
    }
  | BindingFrame
    !(Map Text (Term a))
  | ChooseFrame
    { _chooseTermName :: !Text
    , _chooseSlot     :: !Text
    }

findBinding :: [StackFrame a] -> Text -> Maybe (Term a)
findBinding [] _ = Nothing
findBinding (BindingFrame bindings : stack) name
  = case bindings ^? ix name of
    Just tm -> Just tm
    Nothing -> findBinding stack name
findBinding (_ : stack) name = findBinding stack name

frameVals :: [StackFrame a] -> Map Text (Term a)
frameVals = foldl
  (\sorts -> \case
    BindingFrame bindings -> bindings `Map.union` sorts
    _                     -> sorts)
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
