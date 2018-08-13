{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE PatternSynonyms               #-}
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
  , sortOperators
  , sortVariables
  , Operator(..)
  , Arity(..)
  , Valence(..)
  , exampleArity

  -- * Denotation charts
  -- | Denotational semantics definition.
  , DenotationChart(..)
  , Denotation(..)
  , nameSlot
  , fromSlot
  , toSlot
  , Subst

  -- * Terms / Values
  -- ** Terms
  , Term(..)
  , subterms
  , termName

  -- ** Values
  , Value(..)
  , subvalues
  , valueName

  -- * Patterns
  -- ** Pattern
  , Pattern(..)
  , patternPrimName
  , patternPrimSort
  -- ** PatternCheckResult
  , PatternCheckResult(..)
  , overlapping
  , uncovered
  -- ** Functions
  , applySubst
  , toPattern
  , matches
  , runMatches
  , minus
  , patternCheck
  , findMatch

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

  -- * Tests
  , toPatternTests
  , matchesTests
  , minusTests
  , mkCompletePatternTests
  , allTests
  , eChart
  ) where

import           Control.Lens              hiding (op)
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Zipper
import           Data.Foldable             (fold, foldlM)
import           Data.List                 (intersperse)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               (First(First, getFirst))
import           Data.Sequence             (Seq)
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import           Linguist.Util
import           EasyTest


-- syntax charts

-- | A syntax chart defines the abstract syntax of a language, specified by a
-- collection of operators and their arities. The abstract syntax provides a
-- systematic, unambiguous account of the hierarchical and binding structure of
-- the language.
--
-- @
-- Typ t ::= num                numbers
--           str                strings
--
-- Exp e ::= num[n]             numeral
--           str[s]             literal
--           plus(Exp; Exp)     addition
--           times(Exp; Exp)    multiplication
--           cat(Exp; Exp)      concatenation
--           len(Exp)           length
--           let(Exp; Exp.Exp)  definition
-- @
newtype SyntaxChart = SyntaxChart (Map Text Sort)

-- | Sorts divide ASTs into syntactic categories. For example, programming
-- languages often have a syntactic distinction between expressions and
-- commands.
data Sort = Sort
  { _sortVariables :: ![Text]     -- ^ set of variables
  , _sortOperators :: ![Operator] -- ^ set of operators
  }

-- | One of the fundamental constructions of a language. Operators are grouped
-- into sorts. Each operator has an /arity/, specifying its arguments.
data Operator = Operator
  { _operatorName  :: !Text  -- ^ operator name
  , _operatorArity :: !Arity -- ^ arity
  , _operatorDesc  :: !Text  -- ^ description
  }

-- | An /arity/ specifies the sort of an operator and the number and valences
-- of its arguments.
--
-- eg @(Exp.Exp; Nat)Exp@.
--
-- To specify an arity, the resulting sort is unnecessary. We also include
-- externals.
data Arity
  = Arity    ![Valence]
  | External !Text

-- | A /valence/ specifies the sort of an argument as well as the number and
-- sorts of the variables bound within it.
--
-- eg @Exp.Exp@.
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
newtype DenotationChart a = DenotationChart [(Pattern, Denotation a)]

pattern PatternAny :: Pattern
pattern PatternAny = PatternVar Nothing

pattern PatternEmpty :: Pattern
pattern PatternEmpty = PatternUnion []

-- | A pattern matches a term
data Pattern
  -- | A variable pattern that matches everything, generating a substitution
  = PatternVar  !(Maybe Text)

  -- | Matches the head of a term and any subpatterns
  | PatternTm   !Text ![Pattern]

  -- | Matches 'PrimValue'
  | PatternPrimVal
    { _patternPrimSort :: !Text -- ^ sort, eg "str"
    , _patternPrimName :: !Text -- ^ name, eg "s_1"
    }

  -- | Matches 'PrimTerm'
  | PatternPrimTerm
    { _patternPrimSort :: !Text -- ^ sort, eg "str"
    , _patternPrimName :: !Text -- ^ name, eg "s_1"
    }

  -- TODO: should this exist?
  | BindingPattern ![Text] !Pattern

  -- | The union of patterns
  | PatternUnion ![Pattern]
  deriving (Eq, Show)

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
    , _subvalues :: ![Value a]
    }
  | PrimValue !a
  | Thunk !(Term a)
  deriving Show

data PatternCheckResult a = PatternCheckResult
  { _uncovered   :: !Pattern   -- ^ uncovered patterns
  , _overlapping :: ![Pattern] -- ^ overlapping patterns
  }

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

toPattern :: Operator -> Pattern
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

data MatchesEnv = MatchesEnv
  { _envChart :: !SyntaxChart
  , _envSort  :: !Text
  }

matches :: Pattern -> Term a -> ReaderT MatchesEnv Maybe (Subst a)
matches (PatternVar (Just name)) tm = pure $ Map.singleton name tm
matches (PatternVar Nothing)     _  = pure $ Map.empty

-- matches _ _ pat (Var

matches (PatternTm name1 subpatterns) (Term name2 subterms) = do
  if name1 /= name2
  then lift Nothing
  else do
    MatchesEnv chart sort <- ask
    mMatches <- sequence $ fmap sequence $ pairWith matches subpatterns subterms
    Map.unions <$> lift mMatches
-- TODO: write context of var names?
matches (PatternPrimVal _primName1 _varName) (Return (PrimValue _))
  = pure Map.empty
matches (PatternPrimTerm _primName1 _varName) (PrimTerm _)
  = pure Map.empty
-- TODO: this piece must know the binding structure from the syntax chart
matches (BindingPattern lnames subpat) (Binding rnames subtm)
  -- XXX also match names
  = matches subpat subtm
matches PatternAny _ = pure Map.empty
matches (PatternUnion pats) tm = do
  subMatches <- traverse (fmap Just . (`matches` tm)) pats
  lift $ getFirst $ fold $ fmap First subMatches
matches _ _ = lift Nothing

runMatches :: SyntaxChart -> Text -> ReaderT MatchesEnv Maybe a -> Maybe a
runMatches chart sort = flip runReaderT (MatchesEnv chart sort)

matchesTests :: Test ()
matchesTests = scope "matches" $
  let foo :: Term ()
      foo = Term "foo" []
  in tests
       [ expectJust $ runMatches undefined undefined $ matches (PatternVar (Just "x")) foo
       , expectJust $ runMatches undefined undefined $ matches PatternAny foo
       , expectJust $ runMatches undefined undefined $ matches
         (PatternUnion [PatternAny, undefined]) foo
       ]

-- Chart of the language @e@. We use this for testing.
eChart :: SyntaxChart
eChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    -- TODO: is this the correct arity (sort)?
    [ Operator "num" (Arity []) "numbers"
    , Operator "str" (Arity []) "strings"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "num"   (External "nat") "numeral"
    , Operator "str"   (External "str") "literal"
    , Operator "plus"
      (Arity [Valence [] "Exp", Valence [] "Exp"]) "addition"
    , Operator "times"
      (Arity [Valence [] "Exp", Valence [] "Exp"]) "multiplication"
    , Operator "cat"
      (Arity [Valence [] "Exp", Valence [] "Exp"]) "concatenation"
    , Operator "len"
      (Arity [Valence [] "Exp"]) "length"
    -- TODO:
    -- . the book specifies this arity as
    --   - `let(e1;x.e2)`
    --   - `(Exp, Exp.Exp)Exp`
    -- . is it known that the `x` binds `e1`?
    -- . where is `x` specified?
    , Operator "let"   (Arity [Valence [] "Exp", Valence ["Exp"] "Exp"]) "definition"
    ])
  ]

getSort :: ReaderT MatchesEnv Maybe Sort
getSort = do
  MatchesEnv (SyntaxChart syntax) sort <- ask
  lift $ syntax ^? ix sort

completePattern :: ReaderT MatchesEnv Maybe Pattern
completePattern = do
  MatchesEnv _ sort    <- ask
  Sort _vars operators <- getSort

  let mkPat (Operator opName arity _desc) = case arity of
        Arity valences -> PatternTm opName (const PatternAny <$> valences)
        -- TODO: PatternPrimTerm?
        External name -> PatternPrimVal sort name

  pure $ PatternUnion $ foldr
    (\op pats -> mkPat op : pats)
    []
    operators

mkCompletePatternTests :: Test ()
mkCompletePatternTests = scope "completePattern" $ tests
  [ expect $
      runMatches eChart "Typ" completePattern
      ==
      Just (PatternUnion [PatternTm "num" [], PatternTm "str" []])
  , expect $
      runMatches eChart "Exp" completePattern
      ==
      Just (PatternUnion
        [ PatternPrimVal "Exp" "nat"
        , PatternPrimVal "Exp" "str"
        , PatternTm "plus"  [PatternAny, PatternAny]
        , PatternTm "times" [PatternAny, PatternAny]
        , PatternTm "cat"   [PatternAny, PatternAny]
        , PatternTm "len"   [PatternAny]
        , PatternTm "let"   [PatternAny, PatternAny]
        ])
  ]

minus :: Pattern -> Pattern -> ReaderT MatchesEnv Maybe Pattern
minus _ (PatternVar _) = pure PatternEmpty
minus (PatternVar _) x = do
  pat <- completePattern
  minus pat x

minus x@(PatternTm hd subpats) (PatternTm hd' subpats') =
  if hd == hd'
  then do
    subpats'' <- sequence $ zipWith minus subpats subpats'
    pure $ if all (== PatternEmpty) subpats''
       then PatternEmpty
       else PatternTm hd subpats''
  else pure x

minus (PatternUnion pats) x = PatternUnion <$> traverse (`minus` x) pats

-- remove each pattern from pat one at a time
minus pat (PatternUnion pats) = foldlM minus pat pats

minus x@(PatternPrimVal sort1 _) (PatternPrimVal sort2 _) = pure $
  if sort1 == sort2 then PatternEmpty else x

minus x@(PatternPrimTerm sort1 _) (PatternPrimTerm sort2 _) = pure $
  if sort1 == sort2 then PatternEmpty else x

minus BindingPattern{} BindingPattern{} = error "TODO"

-- these all should return Nothing
minus PatternTm{} PatternPrimVal{}       = error "illegal"
minus PatternTm{} PatternPrimTerm{}      = error "illegal"
minus PatternTm{} BindingPattern{}       = error "illegal"

minus PatternPrimVal{} PatternTm{}       = error "illegal"
minus PatternPrimVal{} PatternPrimTerm{} = error "illegal"
minus PatternPrimVal{} BindingPattern{}  = error "illegal"

minus PatternPrimTerm{} PatternTm{}      = error "illegal"
minus PatternPrimTerm{} PatternPrimVal{} = error "illegal"
minus PatternPrimTerm{} BindingPattern{} = error "illegal"

minus BindingPattern{} PatternTm{}       = error "illegal"
minus BindingPattern{} PatternPrimVal{}  = error "illegal"
minus BindingPattern{} PatternPrimTerm{} = error "illegal"


minusTests :: Test ()
minusTests = scope "minus" $
  let x = PatternVar (Just "x")
      y = PatternVar (Just "y")
      nat = PatternPrimVal "Exp" "nat"
      any' = PatternAny
  in tests
       [ expect $ runMatches undefined undefined (minus x x) == Just PatternEmpty
       , expect $ runMatches undefined undefined (minus x y) == Just PatternEmpty
       , expect $ runMatches undefined undefined (minus x any') == Just PatternEmpty
       -- , expect $ runMatches undefined undefined (minus any' x) == Just PatternEmpty
       , expect $ runMatches undefined undefined (minus any' any') == Just PatternEmpty
       , expect $
         runMatches eChart "Typ" (minus (PatternTm "num" []) (PatternTm "num" []))
         ==
         Just PatternEmpty
       , expect $ runMatches eChart "Exp" (minus nat nat) == Just PatternEmpty
       , expect $
         runMatches eChart "Exp" (minus
           (PatternTm "plus"
             [ PatternPrimVal "Exp" "nat"
             , x
             ])
           (PatternTm "plus"
             [ PatternPrimVal "Exp" "nat"
             , y
             ]))
         ==
         Just PatternEmpty
       ]

patternCheck
  :: forall a. DenotationChart a
  -> ReaderT MatchesEnv Maybe (PatternCheckResult a)
patternCheck (DenotationChart chart) = do
  unmatched <- completePattern -- everything unmatched
  (overlaps, unmatched) <- mapAccumM
    (\unmatchedAcc (pat, _denotation) -> go unmatchedAcc pat)
    unmatched
    chart
  pure $ PatternCheckResult unmatched overlaps

  -- go
  -- . takes the set of uncovered values
  -- . returns the (set of covered values, set of remaining uncovered values)
  where go :: Pattern -> Pattern -> ReaderT MatchesEnv Maybe (Pattern, Pattern)
        -- TODO: are these necessary?
        go _unmatched (PatternVar _) = (, PatternEmpty) <$> completePattern
        go _unmatched PatternAny     = (, PatternEmpty) <$> completePattern
        go unmatched  pat            = (pat, ) <$> unmatched `minus` pat

findMatch
  :: DenotationChart a
  -> Term a
  -> ReaderT MatchesEnv Maybe (Subst a, Denotation a)
findMatch (DenotationChart pats) tm = do
  MatchesEnv chart sort <- ask
  lift $ foldr
    (\(pat, rhs) mat -> case runMatches chart sort (matches pat tm) of
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
    { _frameName  :: !Text                       -- ^ name of this term
    , _frameVals  :: !(Seq (Value a))            -- ^ values before
    , _frameTerms :: ![Term a]                   -- ^ subterms after
    , _frameK     :: !(Seq (Value a) -> Value a) -- ^ what to do after
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


allTests :: Test ()
allTests = scope "all tests" $ tests
  [ toPatternTests
  , matchesTests
  , minusTests
  , mkCompletePatternTests
  ]


makeLenses ''Sort
makeLenses ''Pattern
makeLenses ''Denotation
makeLenses ''Term
makeLenses ''Value
makeLenses ''PatternCheckResult
