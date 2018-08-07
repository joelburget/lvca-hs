{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2018 Joel Burget
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Joel Burget <joelburget@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Tools for defining the syntax and semantics of languages.

module Linguist.Types
  ( -- * Syntax charts
    -- | Syntax definition.
    SyntaxChart(..)
  , Sort(..)
  , Operator(..)
  , Arity(..)
  , Valence(..)
  , exampleArity
  , sortOperators
  , sortVariables
  , matchSetExternals
  , matchSetTerms

  -- * Denotation charts
  -- | Denotational semantics definition.
  , DenotationChart(..)
  , Pattern(..)
  , Denotation(..)
  , Subst
  , Term(..)
  , Value(..)
  , PatternCheckResult(..)
  , MatchSet(..)
  , applySubst
  , toPattern
  , toPatternTests
  , matches
  , patternToMatchSet
  , emptyMatchSet
  , mkCompleteMatchSet
  , minus
  , patternCheck
  , findMatch
  , nameSlot
  , fromSlot
  , toSlot

  -- * Judgements
  , InOut(..)
  , JudgementForm(..)
  , OperatorApplication(..)
  , SaturatedTerm(..)
  , JudgementClause(..)
  , JudgementRule(..)
  , JudgementRules(..)
  , (@@)
  , (@@@)
  , (%%%)
  , (.--)

  -- * Evaluation
  , StackFrame(..)
  , findBinding
  , StateStep(..)
  , State
  , ListZipper
  , next
  , prev
  ) where

import           Control.Lens
import           Control.Monad             (join)
import           Control.Zipper
import           Data.List                 (intersperse, mapAccumL)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Sequence             (Seq)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import           Linguist.Util
import           EasyTest

-- syntax charts

newtype SyntaxChart = SyntaxChart (Map Text Sort)

data Sort = Sort
  { _sortVariables :: ![Text]     -- ^ set of variables
  , _sortOperators :: ![Operator] -- ^ set of operators
  }

data Operator = Operator
  { _operatorName  :: !Text  -- ^ operator name
  , _operatorArity :: !Arity -- ^ arity
  , _operatorDesc  :: !Text  -- ^ description
  }

-- | Arity, eg @(Exp.Exp; Nat)Exp@.
--
-- To specify an arity, the resulting sort is unnecessary. We also include
-- externals.
data Arity
  = Arity    ![Valence]
  | External !Text

-- | Valence, eg @Exp.Exp@.
data Valence = Valence
  { _valenceSorts  :: ![Text] -- ^ the sorts of all bound variables
  , _valenceResult :: !Text   -- ^ the resulting sort
  }

-- | @exampleArity = 'Arity' ['Valence' [\"Exp\", \"Exp\"] \"Exp\"]@
exampleArity :: Arity
exampleArity = Arity [Valence ["Exp", "Exp"] "Exp"]


-- | Denotation charts
--
-- A denotation chart maps from patterns to their denotation. Patterns are
-- checked from top to bottom.
--
-- We check for completeness and reduncancy using a very similar algorithm to
-- Haskell's pattern match checks. These could also be compiled efficiently in
-- a similar way.
newtype DenotationChart a = DenotationChart [(Pattern a, Denotation a)]

-- | A pattern matches a term
data Pattern a
  -- | A variable pattern that matches everything, generating a substitution
  = PatternVar  !Text

  -- | Matches the head of a term and any subpatterns
  | PatternTm   !Text ![Pattern a]

  | PatternPrimVal
    { _patternPrimSort :: !Text -- ^ sort, eg "str"
    , _patternPrimName :: !Text -- ^ name, eg "s_1"
    }

  | PatternPrimTerm
    { _patternPrimSort :: !Text -- ^ sort, eg "str"
    , _patternPrimName :: !Text -- ^ name, eg "s_1"
    }

  | BindingPattern ![Text] (Pattern a)

  -- | Matches anything without binding
  | PatternAny
  deriving Eq

data Denotation a
  = Value
  | CallForeign !(Seq (Value a) -> Value a)
  | BindIn
    { _nameSlot :: !Text
    , _fromSlot :: !Text
    , _toSlot   :: !Text
    }

type Subst a = Map Text (Term a)

-- | A term, or unevaluated expression
data Term a
  = Term
    { _termName :: !Text     -- ^ name of this term
    , _subterms :: ![Term a] -- ^ subterms
    }
  | Binding
    ![Text]
    !(Term a)
  | Var !Text
  | PrimTerm !a
  | Return !(Value a)
  deriving Show

-- | A value, or evaluated expression
data Value a
  = NativeValue
    { _valueName :: !Text    -- constructor(?) name
    , _subValues :: ![Value a]
    }
  | PrimValue !a
  | Thunk !(Term a)
  deriving Show

data PatternCheckResult a = PatternCheckResult
  { _uncovered   :: !(MatchSet a) -- ^ uncovered patterns
  , _overlapping :: ![MatchSet a] -- ^ overlapping patterns
  }

-- TODO: What about different sorts?
data MatchSet a = MatchSet
  { _matchSetTerms     :: !(Map Text [MatchSet a])
  , _matchSetExternals :: !(Set Text)
  } | CompleteMatchSet

type instance Index   (Term a) = Int
type instance IxValue (Term a) = Term a
instance Ixed (Term a) where
  ix k f tm = case tm of
    Term name tms -> Term name <$> ix k f tms
    _             -> pure tm

type instance Index   (Value a) = Int
type instance IxValue (Value a) = Value a
instance Ixed (Value a) where
  ix k f tm = case tm of
    NativeValue name tms -> NativeValue name <$> ix k f tms
    _                    -> pure tm

applySubst :: Subst a -> Term a -> Term a
applySubst subst tm = case tm of
  Term name subtms    -> Term name (applySubst subst <$> subtms)
  Binding names subtm -> Binding names (applySubst subst subtm)
  _                   -> tm

toPattern :: Operator -> Pattern a
toPattern (Operator name arity _desc) = PatternTm name $ case arity of
  Arity valences -> const PatternAny <$> valences
  External _     -> [PatternAny]

toPatternTests :: Test ()
toPatternTests = scope "toPattern" $ tests
  [ expect $
    toPattern (Operator "num" (Arity []) "numbers")
    ==
    PatternTm "num" []
  , expect $
    toPattern (Operator "plus"  (Arity [Valence [] "Exp", Valence [] "Exp"]) "addition")
    ==
    PatternTm "plus" [PatternAny, PatternAny]
  , expect $
    toPattern (Operator "num" (External "nat") "numeral")
    ==
    PatternTm "num" [PatternAny]
  ]

-- TODO: reader
matches :: SyntaxChart -> Text -> Pattern a -> Term a -> Maybe (Subst a)
matches _ _ (PatternVar name) tm = Just $ Map.singleton name tm
matches chart sort (PatternTm name1 subpatterns) (Term name2 subterms) =
  if name1 /= name2
  then Nothing
  else fmap Map.unions $ join $ fmap sequence $
    pairWith (matches chart sort) subpatterns subterms
-- TODO: write context of var names?
matches _ _ (PatternPrimVal _primName1 _varName) (Return (PrimValue _)) = Just Map.empty
matches _ _ (PatternPrimTerm _primName1 _varName) (PrimTerm _) = Just Map.empty
-- TODO: this piece must know the binding structure from the syntax chart
matches chart sort (BindingPattern lnames subpat) (Binding rnames subtm) =
  -- XXX also match names
  matches chart sort subpat subtm
matches _ _ PatternAny _ = Just Map.empty
matches _ _ _ _ = Nothing

patternToMatchSet :: Pattern a -> MatchSet a
patternToMatchSet = \case
  PatternVar _ -> CompleteMatchSet
  PatternAny   -> CompleteMatchSet
  PatternTm name subpats -> MatchSet
    (Map.singleton name (fmap patternToMatchSet subpats))
    Set.empty
  PatternPrimVal sort _ -> MatchSet Map.empty (Set.singleton sort)
  PatternPrimTerm sort _ -> MatchSet Map.empty (Set.singleton sort)
  -- not sure about this one
  BindingPattern _ pat -> patternToMatchSet pat

emptyMatchSet :: MatchSet a
emptyMatchSet = MatchSet Map.empty Set.empty

-- TODO: make partial if we don't find the sort
-- TODO: why not just CompleteMatchSet?
-- TODO: reader
mkCompleteMatchSet :: SyntaxChart -> Text -> MatchSet a
mkCompleteMatchSet (SyntaxChart syntax) sort =
  let (Sort _vars operators) = syntax Map.! sort
  in foldl
    (\(MatchSet tms externals) (Operator opName arity _desc) -> case arity of
      Arity valences -> MatchSet
        (Map.insert opName (const CompleteMatchSet <$> valences) tms)
        externals
      External name -> MatchSet tms (Set.insert name externals)
    )
    emptyMatchSet
    operators

-- TODO: reader
minus :: SyntaxChart -> Text -> MatchSet a -> MatchSet a -> MatchSet a
minus chart sort (MatchSet ts1 xs1) (MatchSet ts2 xs2) = MatchSet
  -- re unionWith: we expect both sides to have the same keys
  (Map.unionWith (zipWith (minus chart sort)) ts1 ts2)
  (Set.difference xs1 xs2)
minus chart sort ms@(MatchSet _ _) CompleteMatchSet
  = minus chart sort ms (mkCompleteMatchSet chart sort)
minus chart sort CompleteMatchSet ms@(MatchSet _ _)
  = minus chart sort (mkCompleteMatchSet chart sort) ms
minus _chart _sort CompleteMatchSet CompleteMatchSet = emptyMatchSet

-- TODO: reader
patternCheck :: forall a. Text -> SyntaxChart -> DenotationChart a -> PatternCheckResult a
patternCheck sort syntax (DenotationChart chart) =
  let (unmatched, overlaps) = mapAccumL
        (\unmatchedAcc (pat, _denotation) -> go unmatchedAcc pat)
        completeMatchSet -- everything unmatched
        chart
  in PatternCheckResult unmatched overlaps

  -- go
  -- . takes the set of uncovered values
  -- . returns the (set of covered values, set of remaining uncovered values)
  where go :: MatchSet a -> Pattern a -> (MatchSet a, MatchSet a)
        -- TODO: are these necessary?
        go _unmatched (PatternVar _) = (completeMatchSet, emptyMatchSet)
        go _unmatched PatternAny     = (completeMatchSet, emptyMatchSet)
        go unmatched pat =
          let ms = patternToMatchSet pat
              minus' = minus syntax sort
          in (ms, unmatched `minus'` ms)

        completeMatchSet :: MatchSet a
        completeMatchSet = mkCompleteMatchSet syntax sort

-- TODO: reader
findMatch
  :: SyntaxChart -> Text -> DenotationChart a -> Term a -> Maybe (Subst a, Denotation a)
findMatch chart sort (DenotationChart pats) tm = foldr
  (\(pat, rhs) mat -> case matches chart sort pat tm of
    Just subst -> Just (subst, rhs)
    Nothing    -> mat)
  Nothing
  pats

-- judgements

data InOut = In | Out

data JudgementForm = JudgementForm
  { _judgementName  :: !Text            -- ^ name of the judgement
  , _judgementSlots :: ![(InOut, Text)] -- ^ mode and sort of all slots
  }

data OperatorApplication = OperatorApplication
  { _operatorApName :: !Text            -- ^ operator name
  , _applicands     :: ![SaturatedTerm] -- applicands
  }

data SaturatedTerm
  = JVariable Text
  | Op OperatorApplication

instance IsString SaturatedTerm where
  fromString = JVariable . fromString

infix 2 @@
(@@) :: Text -> [SaturatedTerm] -> SaturatedTerm
(@@) a b = Op (OperatorApplication a b)

data JudgementClause = JudgementClause
  { _judgementHead       :: !Text            -- ^ head (name of judgement)
  , _judgementApplicands :: ![SaturatedTerm] -- ^ applicands
  }

infix 2 @@@
(@@@) :: Text -> [SaturatedTerm] -> JudgementClause
(@@@) = JudgementClause

infix 2 %%%
(%%%) :: [SaturatedTerm] -> Text -> JudgementClause
(%%%) = flip JudgementClause

data JudgementRule = JudgementRule
  { _assumptions :: ![JudgementClause] -- ^ assumptions
  , _conclusion  :: !JudgementClause   -- ^ conclusion
  }

infix 0 .--
(.--) :: [JudgementClause] -> JudgementClause -> JudgementRule
(.--) = JudgementRule

newtype JudgementRules = JudgementRules [JudgementRule]

instance Pretty OperatorApplication where
  pretty (OperatorApplication hd applicands)
    = pretty hd PP.<+> hsep (pretty <$> applicands)

instance Pretty SaturatedTerm where
  pretty = \case
    JVariable name -> pretty name
    Op opAp        -> pretty opAp

instance Pretty JudgementClause where
  pretty (JudgementClause hd applicands)
    = pretty hd PP.<+> hsep (pretty <$> applicands)

instance Pretty JudgementRule where
  pretty (JudgementRule assumptions conclusion) = vsep
    [ hsep (pretty <$> assumptions)
    , "------"
    , pretty conclusion
    ]

instance Pretty JudgementRules where
  pretty (JudgementRules rules) = vsep (intersperse "" (pretty <$> rules))

instance Pretty SyntaxChart where
  pretty (SyntaxChart sorts) =
    let f (title, operators) = vsep [pretty title <> " ::=", indent 2 $ pretty operators]
    in vsep (f <$> Map.toList sorts)

instance Pretty Sort where
  pretty (Sort _vars operators)
    = vsep (pretty <$> operators)

instance Pretty Operator where
  pretty (Operator name arity _desc)
    = hsep [pretty name <> ":", pretty arity]

-- instance Pretty Arity where
--   pretty (Arity valences result) =
--     let valences' = if null valences
--           then mempty
--           else parens (hsep (punctuate comma (pretty <$> valences)))
--     in valences' <> pretty result

instance Pretty Arity where
  pretty (Arity valences) =
    if null valences
    then mempty
    else parens (hsep (punctuate comma (pretty <$> valences)))
  pretty (External name) = pretty name

instance Pretty Valence where
  pretty (Valence boundVars result) = hsep $
    punctuate dot (fmap pretty boundVars <> [pretty result])


-- evaluation internals

-- thunk  : computation -> value
-- force  : value       -> computation
-- return : value       -> computation

data StackFrame a
  = CbvFrame
    { _frameName  :: !Text                    -- ^ name of this term
    , _frameVals  :: !(Seq (Value a))         -- ^ values before
    , _frameTerms :: ![Term a]                -- ^ subterms after
    , _frameK     :: !(Seq (Value a) -> Value a)  -- ^ what to do after
    }
  | BindingFrame
    !(Map Text (Either (Term a) (Value a)))

findBinding :: [StackFrame a] -> Text -> Maybe (Either (Term a) (Value a))
findBinding [] _ = Nothing
findBinding (BindingFrame bindings : stack) name
  = case bindings ^? ix name of
    Just tmVal -> Just tmVal
    Nothing    -> findBinding stack name
findBinding (_ : stack) name = findBinding stack name

data StateStep a = StateStep
  { _stepFrames ::  ![StackFrame a]
  , _stepFocus  :: !(Term a) -- ^ Either descending into term or ascending with value
  }

  | Errored !Text
  | Done !(Value a)

type State = ListZipper (StateStep (Either Int Text))

type ListZipper a = Top :>> [a] :>> a

next :: State -> State
next state = case state ^. focus of
  Done{} -> state
  _      -> case state & rightward of
    Just state' -> state'
    Nothing     -> state

prev :: State -> State
prev state = case state & leftward of
  Just state' -> state'
  Nothing     -> state


makeLenses ''Sort
makeLenses ''Pattern
makeLenses ''Denotation
makeLenses ''Term
makeLenses ''Value
makeLenses ''PatternCheckResult
makeLenses ''MatchSet
