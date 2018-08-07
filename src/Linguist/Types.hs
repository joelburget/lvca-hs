{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell     #-}
module Linguist.Types where

import           Data.Maybe (mapMaybe)
import           Control.Lens
import           Control.Monad (join)
import           Control.Zipper
import           Data.List                 (intersperse, mapAccumL)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Sequence             (Seq)
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Set (Set)
import qualified Data.Set as Set

-- syntax charts

newtype SyntaxChart = SyntaxChart (Map Text Sort)

data Sort = Sort
  -- !Text       -- ^ name of the sort
  { _sortVariables :: ![Text]     -- ^ set of variables
  , _sortOperators :: ![Operator] -- ^ set of operators
  }

data Operator = Operator
  !Text      -- ^ operator name
  !AritySpec -- ^ arity
  !Text      -- ^ description

toPattern :: Operator -> Maybe (Pattern a)
toPattern (Operator name arity _desc) = case arity of
  Arity' valences -> Just $ PatternTm name $ const PatternAny <$> valences
  VariableArity x -> Nothing
  External name   -> Just $ PatternTm name [PatternAny]

data Arity = Arity
  ![Valence] -- ^ the valences
  !Text      -- ^ the resulting sort

data Valence = Valence
  ![Text] -- ^ the sorts of all bound variables
  Text    -- ^ the resulting sort

-- To specify an arity, the resulting sort is unnecessary. We also include
-- variables and externals.
data AritySpec
  = Arity'        ![Valence]
  | VariableArity !Text
  | External      !Text


-- denotation charts

newtype DenotationChart a = DenotationChart [(Pattern a, Denotation a)]

data Pattern a
  -- | A variable pattern that matches everything, generating a substitution
  = PatternVar  !Text

  -- | Matches the head of a term and any subpatterns
  | PatternTm   !Text ![Pattern a]

  | PatternPrimVal
    !Text -- ^ sort, eg "str"
    !Text -- ^ name, eg "s_1"

  | PatternPrimTerm
    !Text -- ^ sort, eg "str"
    !Text -- ^ name, eg "s_1"

  | BindingPattern ![Text] (Pattern a)

  | PatternAny

data Denotation a
  = Value
  | CallForeign !(Seq (Value a) -> Value a)
  | BindIn !Int !Int !Int

type Subst a = Map Text (Term a)

data Term a
  = Term
    !Text     -- ^ name of this term
    ![Term a] -- ^ subterms
  | Binding
    ![Text]
    !(Term a)
  | Var !Text
  | PrimTerm !a
  | Return !(Value a)
  deriving Show

data Value a
  = NativeValue
    !Text    -- constructor(?) name
    ![Value a]
  | PrimValue !a
  | Thunk !(Term a)
  deriving Show

data PatternCheckResult a = PatternCheckResult
  !(MatchSet a) -- ^ uncovered patterns
  ![MatchSet a] -- ^ overlapping patterns

-- TODO: What about different sorts?
data MatchSet a = MatchSet
  { _matchSetTerms     :: !(Map Text [MatchSet a])
  , _matchSetExternals :: !(Set Text)
  } | CompleteMatchSet

makeLenses ''Sort
makeLenses ''MatchSet

pair :: [a] -> [b] -> Maybe [(a, b)]
pair [] [] = Just []
pair (a:as) (b:bs) = ((a, b):) <$> pair as bs
pair _ _ = Nothing

pairWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pairWith _f []     []     = Just []
pairWith f  (a:as) (b:bs) = (f a b :) <$> pairWith f as bs
pairWith _ _ _ = Nothing

matches :: Pattern a -> Term a -> Maybe (Subst a)
matches (PatternVar name) tm = Just $ Map.singleton name tm
matches (PatternTm name1 subpatterns) (Term name2 subterms) =
  if name1 /= name2
  then Nothing
  else fmap Map.unions $ join $ fmap sequence $ pairWith matches subpatterns subterms
-- TODO: write context of var names?
matches (PatternPrimVal _primName1 _varName) (Return (PrimValue _)) = Just Map.empty
matches (PatternPrimTerm _primName1 _varName) (PrimTerm _) = Just Map.empty
-- TODO: this piece must know the binding structure from the syntax chart
matches (BindingPattern lnames subpat) (Binding rnames subtm) =
  -- XXX also match names
  matches subpat subtm
matches PatternAny _ = Just Map.empty
matches _ _ = Nothing

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

emptyMatchSet  :: MatchSet a
emptyMatchSet    = MatchSet Map.empty Set.empty

-- TODO: make partial if we don't find the sort
-- TODO: why not just CompleteMatchSet?
mkCompleteMatchSet :: SyntaxChart -> Text -> MatchSet a
mkCompleteMatchSet (SyntaxChart syntax) sort =
  Map.foldlWithKey
    (\accMS sortName (Sort _vars operators) -> foldl
      (\ms@(MatchSet tms externals) (Operator opName arity _desc) -> case arity of
        Arity' valences -> MatchSet
          (Map.insert opName (const CompleteMatchSet <$> valences) tms)
          externals
        VariableArity name -> ms
        External name -> MatchSet tms (Set.insert name externals)
      )
      accMS
      operators
    )
    emptyMatchSet
    syntax

minus :: MatchSet a -> MatchSet a -> MatchSet a
minus (MatchSet ts1 xs1) (MatchSet ts2 xs2) = MatchSet
  -- re unionWith: we expect both sides to have the same keys
  (Map.unionWith (zipWith minus) ts1 ts2)
  (Set.difference xs1 xs2)
minus (MatchSet ts1 xs1) CompleteMatchSet = undefined
minus CompleteMatchSet (MatchSet ts2 xs2) = undefined
minus CompleteMatchSet CompleteMatchSet = undefined

patternCheck :: forall a. Text -> SyntaxChart -> DenotationChart a -> PatternCheckResult a
patternCheck sort syntax (DenotationChart chart) =
  let (unmatched, overlaps) = mapAccumL
        (\unmatchedAcc (pat, _denotation) -> go unmatchedAcc pat)
        completeMatchSet -- everything unmatched
        chart
  in PatternCheckResult unmatched overlaps

  -- go
  -- * takes the set of uncovered values
  -- * returns the (set of covered values, set of remaining uncovered values)
  where go :: MatchSet a -> Pattern a -> (MatchSet a, MatchSet a)
        go _unmatched (PatternVar _) = (completeMatchSet, emptyMatchSet)
        go _unmatched PatternAny     = (completeMatchSet, emptyMatchSet)
        go unmatched pat =
          let ms = patternToMatchSet pat
          in (ms, unmatched `minus` ms)
        -- go unmatched (PatternTm name subterms)      =
        -- go unmatched (PatternPrimVal sort name)     =
        -- go unmatched (PatternPrimTerm sort name)    =
        -- go unmatched (BindingPattern binds subterm) =

        -- remove the second pattern from the first, returning the set of
        -- values covered
        remove :: Pattern a -> Pattern a -> (MatchSet a, [Pattern a])
        remove = undefined

        completeMatchSet :: MatchSet a
        completeMatchSet = mkCompleteMatchSet syntax sort

findMatch :: DenotationChart a -> Term a -> Maybe (Subst a, Denotation a)
findMatch (DenotationChart pats) tm = foldr
  (\(pat, rhs) mat -> case matches pat tm of
    Just subst -> Just (subst, rhs)
    Nothing    -> mat)
  Nothing
  pats

-- judgements

data InOut = In | Out

data JudgementForm = JudgementForm
  !Text            -- ^ name of the judgement
  ![(InOut, Text)] -- ^ mode and sort of all slots

data OperatorApplication = OperatorApplication
  !Text            -- ^ operator name
  ![SaturatedTerm] -- applicands

data SaturatedTerm
  = JVariable Text
  | Op OperatorApplication

instance IsString SaturatedTerm where
  fromString = JVariable . fromString

infix 2 @@
(@@) :: Text -> [SaturatedTerm] -> SaturatedTerm
(@@) a b = Op (OperatorApplication a b)

data JudgementClause = JudgementClause
  !Text            -- ^ head (name of judgement)
  ![SaturatedTerm] -- ^ applicands

infix 2 @@@
(@@@) :: Text -> [SaturatedTerm] -> JudgementClause
(@@@) = JudgementClause

infix 2 %%%
(%%%) :: [SaturatedTerm] -> Text -> JudgementClause
(%%%) = flip JudgementClause

data JudgementRule = JudgementRule
  ![JudgementClause] -- ^ assumptions
  !JudgementClause   -- ^ conclusion

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

instance Pretty Arity where
  pretty (Arity valences result) =
    let valences' = if null valences
          then mempty
          else parens (hsep (punctuate comma (pretty <$> valences)))
    in valences' <> pretty result

instance Pretty AritySpec where
  pretty (Arity' valences) =
    if null valences
    then mempty
    else parens (hsep (punctuate comma (pretty <$> valences)))
  pretty (VariableArity name) = pretty name
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
    !Text                    -- ^ name of this term
    !(Seq (Value a))         -- ^ values before
    ![Term a]                -- ^ subterms after
    !(Seq (Value a) -> Value a)  -- ^ what to do after
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
  ![StackFrame a]
  !(Term a) -- ^ Either descending into term or ascending with value

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
