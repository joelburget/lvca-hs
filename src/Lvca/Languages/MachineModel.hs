{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Lvca.Languages.MachineModel
  ( MachineF(..)
  , machineP

  , denotationChartP
  , mkDenotationChart
  , unMkDenotationChart
  , machineTermP

  -- * Evaluation
  , StackFrame(..)
  -- , findBinding
  -- , frameVals
  , StateStep(..)
  , Focus(..)
  , Extended
  , Extended'
  ) where

import           Data.Traversable          (for)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Control.Lens
  (Prism', _1, _2, (%~), preview, review, prism', (&))
import           Data.Text                 (Text)

import Lvca.FunctorUtil
import Lvca.Types
import Lvca.Util

-- mkTypes (Options Nothing $ Map.singleton "Text" (ConT ''Text))
--   "Machine ::=                                                              \n\
--   \  Lam'(Machine)                                                          \n\
--   \  App'(Machine; Machine)                                                 \n\
--   \  PrimApp'({Text}; List(Machine))"
-- mkSyntaxInstances ''Machine

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
     Prism' (Term a) Text
  -> Prism' (Term a)           (Fix f)
  -> Prism' (Term a) (MachineF (Fix f))
machineP textP p = prism' rtl ltr where
  rtl :: MachineF (Fix f) -> Term a
  rtl = \case
    Lam body -> Fix $ Term "Lam"
      [ review p body
      ]
    App body arg -> Fix $ Term "App"
      [ review p body
      , review p arg
      ]
    PrimApp name args -> Fix $ Term "PrimApp"
      [ review textP     name
      , review (listP p) args
      ]

  ltr :: Term a -> Maybe (MachineF (Fix f))
  ltr (Fix tm) = case tm of
    Term "Lam" [body] -> Lam
      <$> preview p body
    Term "App" [body, arg] -> App
      <$> preview p body
      <*> preview p arg
    Term "PrimApp" [name, args] -> PrimApp
      <$> preview textP     name
      <*> preview (listP p) args
    _ -> Nothing

mkDenotationChart
  :: Prism' (Pattern a) (Fix (PatVarF     :+: f))
  -> Prism' (Term    b) (Fix (VarBindingF :+: MeaningOfF :+: g))
  -> DenotationChart' f g
  -> DenotationChart  a b
mkDenotationChart patP' termP' (DenotationChart' rules) = DenotationChart $
  rules & traverse . _1 %~ review patP'
        & traverse . _2 %~ review termP'

unMkDenotationChart
  :: (Show1 f, Show a, Show1 g, Show b)
  => Prism' (Pattern a) (Fix (PatVarF     :+: f))
  -> Prism' (Term    b) (Fix (VarBindingF :+: MeaningOfF :+: g))
  ->        DenotationChart  a b
  -> Maybe (DenotationChart' f g)
unMkDenotationChart patP' termP' (DenotationChart rules)
  = fmap DenotationChart' $ for rules $ \(a, b)
    -> (,) <$> preview patP'  a <*> preview termP' b

denotationChartP
  :: (Show1 f, Show a, Show1 g, Show b)
  => Prism' (Pattern a) (Fix (PatVarF     :+: f))
  -> Prism' (Term    b) (Fix (VarBindingF :+: MeaningOfF :+: g))
  -> Prism' (DenotationChart a b) (DenotationChart' f g)
denotationChartP patP' termP' = prism'
  (mkDenotationChart   patP' termP')
  (unMkDenotationChart patP' termP')

-- TODO: find a better place to put this
listP :: Prism' (Term a) b -> Prism' (Term a) [b]
listP p = prism' rtl ltr where
  rtl = \case
    []   -> Fix $ Term "Nil"  []
    x:xs -> Fix $ Term "Cons" [ review p x , rtl xs ]

  ltr (Fix tm) = case tm of
    Term "Nil"  []      -> Just []
    Term "Cons" [x, xs] -> (:) <$> preview p x <*> ltr xs
    _ -> Nothing

machineTermP
  :: forall a f.
     TermRepresentable f
  => Prism' (Term a) Text
  -> Prism'
       (Term a)
       (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: f a))
machineTermP textP = sumPrisms4'
  varBindingP
  (\_p ->  meaningOfP textP)
  (machineP textP )
  mkTermP

-- evaluation internals

type Extended  f a = VarBindingF :+: MachineF :+: f a
type Extended' f a = Extended f (Either Text a)

data StackFrame f a
  = EvalFrame !(f a ()) ![Maybe (Fix (Extended' f a))]
  -- ^ A term with holes at the position of each child + each child. @Nothing@
  -- marks the location where we're currently working.
  | BindingFrame !Text !(Fix (f a)) -- !(Fix (Extended' f a))
  -- ^ Binding the name to the given value in the subterm

instance (Show2 f, Show1 (f a), Show a) => Show (StackFrame f a) where
  showsPrec p frame = showParen (p > 10) $ case frame of
    EvalFrame frame' _tms ->
        showString "EvalFrame "
      . showsPrec2 11 frame'
      . showString " TODO"
      -- . showsPrec 11 tms
    BindingFrame name tm ->
        showString "BindingFrame "
      . showsPrec 11 name
      . showChar ' '
      . showsPrec 11 tm

-- findBinding :: [StackFrame a] -> Text -> Maybe (Term a)
-- findBinding [] _ = Nothing
-- findBinding (BindingFrame bname tm : stack) name
--   | bname == name = Just tm
--   | otherwise = findBinding stack name
-- findBinding (_ : stack) name = findBinding stack name

-- frameVals :: [StackFrame a] -> Map Text (Term a)
-- frameVals = foldl
--   (\sorts -> \case
--     BindingFrame name tm -> Map.insert name tm sorts
--     _                    -> sorts)
--   Map.empty

-- TODO: include note like RETURNING / SUBST, etc
data StateStep f a = StateStep
  { _stepFrames :: ![StackFrame f a]
  -- | Either descending into term or ascending with value
  , _stepFocus  :: !(Focus f a)
  }

  | Errored !Text
  | Done !(Fix (f a))
  deriving Show

data Focus f a
  = Descending !(Fix (Extended' f a))
  | Ascending  !(Fix (f a))

instance (Show2 f, Show1 (f a), Show a) => Show (Focus f a) where
  showsPrec p focus = showParen (p > 10) $ case focus of
    Descending _ -> showString "Descending (TODO)"
    Ascending tm -> showString "Ascending" . showsPrec 11 tm
