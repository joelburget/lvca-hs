{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
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
    SortName
  , SyntaxChart(..)
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
  , pattern PatternAny
  , pattern PatternEmpty
  , Denotation(..)
  , nameSlot
  , fromSlot
  , toSlot
  , bodySlot
  , argSlot
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

  , TmVal

  -- * Patterns
  -- ** Pattern
  , Pattern(..)
  , patternPrimName
  , patternPrimSort
  -- ** PatternCheckResult
  , PatternCheckResult(..)
  , overlapping
  , uncovered
  , isComplete
  , hasRedundantPat
  -- ** Functions
  , applySubst
  , toPattern
  , MatchesEnv(..)
  , envChart
  , envSort
  , envVars
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
  , frameVals
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
import           Data.Monoid               (First (First, getFirst))
import           Data.Sequence             (Seq)
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import           EasyTest
import           Linguist.Util


-- syntax charts

type SortName = Text

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
newtype SyntaxChart = SyntaxChart (Map SortName Sort)

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
  | External !SortName

-- | A /valence/ specifies the sort of an argument as well as the number and
-- sorts of the variables bound within it.
--
-- eg @Exp.Exp@.
data Valence = Valence
  { _valenceSorts  :: ![SortName] -- ^ the sorts of all bound variables
  , _valenceResult :: !SortName   -- ^ the resulting sort
  }

instance IsString Valence where
  fromString = Valence [] . fromString

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
    { _patternPrimSort :: !SortName -- ^ sort, eg "str"
    , _patternPrimName :: !Text     -- ^ name, eg "s_1"
    }

  -- | Matches 'PrimTerm'
  | PatternPrimTerm
    { _patternPrimSort :: !SortName -- ^ sort, eg "str"
    , _patternPrimName :: !Text     -- ^ name, eg "s_1"
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
  | Cbv
    { _bodySlot :: !Text
    , _argSlot  :: !Text
    }
  | Cbn
    { _bodySlot :: !Text
    , _argSlot  :: !Text
    }

type Subst a = Map Text (TmVal a)

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

type TmVal a = Either (Term a) (Value a)

data IsRedudant = IsRedudant | IsntRedundant
  deriving Eq

data PatternCheckResult a = PatternCheckResult
  { _uncovered   :: !Pattern                 -- ^ uncovered patterns
  , _overlapping :: ![(Pattern, IsRedudant)] -- ^ overlapping part, redundant?
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

isComplete :: PatternCheckResult a -> Bool
isComplete (PatternCheckResult uc _) = uc == PatternUnion []

hasRedundantPat :: PatternCheckResult a -> Bool
hasRedundantPat (PatternCheckResult _ overlaps)
  = any ((== IsRedudant) . snd) overlaps

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
    toPattern (Operator "plus"  (Arity ["Exp", "Exp"]) "addition")
    ==
    PatternTm "plus" [PatternAny, PatternAny]
  , expect $
    toPattern (Operator "num" (External "nat") "numeral")
    ==
    PatternTm "num" [PatternAny]
  ]

data MatchesEnv a = MatchesEnv
  { _envChart :: !SyntaxChart
  , _envSort  :: !SortName
  , _envVars  :: !(Map Text (TmVal a))
  }

makeLenses ''MatchesEnv

type Matching a = ReaderT (MatchesEnv a) Maybe

noMatch, emptyMatch :: Matching a (Subst a)
noMatch = lift Nothing
emptyMatch = pure Map.empty

matches :: Pattern -> TmVal a -> Matching a (Subst a)
matches pat (Left (Return val))     = matches pat (Right val)
matches pat (Right (Thunk tm))      = matches pat (Left tm)
matches (PatternVar (Just name)) tm = pure $ Map.singleton name tm
matches (PatternVar Nothing)     _  = emptyMatch

matches pat (Left (Var name)) = do
  mTermVal <- view $ envVars . at name
  case mTermVal of
    Just termVal -> matches pat termVal
    Nothing      -> noMatch

matches (PatternTm name1 subpatterns) (Left (Term name2 subterms)) = do
  if name1 /= name2
  then noMatch
  else do
    mMatches <- sequence $ fmap sequence $
      pairWith matches subpatterns (fmap Left subterms)
    Map.unions <$> lift mMatches
-- TODO: write context of var names?
matches (PatternPrimVal _primName1 _varName) (Right (PrimValue _))
  = emptyMatch
matches (PatternPrimTerm _primName1 _varName) (Left (PrimTerm _))
  = emptyMatch
-- TODO: this piece must know the binding structure from the syntax chart
matches (BindingPattern lnames subpat) (Left (Binding rnames subtm))
  -- XXX also match names
  = matches subpat (Left subtm)
matches PatternAny _ = emptyMatch
matches (PatternUnion pats) tm = do
  env <- ask
  lift $ getFirst $ foldMap
    (\pat -> First $ runReaderT (pat `matches` tm) env)
    pats
matches _ _ = noMatch

runMatches :: SyntaxChart -> SortName -> Matching b a -> Maybe a
runMatches chart sort = flip runReaderT (MatchesEnv chart sort Map.empty)

matchesTests :: Test ()
matchesTests = scope "matches" $
  let foo :: Term ()
      foo = Term "foo" []
  in tests
       [ expectJust $ runMatches undefined undefined $ matches
         (PatternVar (Just "x")) (Left foo)
       , expectJust $ runMatches undefined undefined $ matches
         PatternAny (Left foo)
       , expectJust $ runMatches undefined undefined $ matches
         (PatternUnion [PatternAny, undefined]) (Left foo)
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
    [ Operator "num"   (External "num") "numeral"
    , Operator "str"   (External "str") "literal"
    , Operator "plus"
      (Arity ["Exp", "Exp"]) "addition"
    , Operator "times"
      (Arity ["Exp", "Exp"]) "multiplication"
    , Operator "cat"
      (Arity ["Exp", "Exp"]) "concatenation"
    , Operator "len"
      (Arity ["Exp"]) "length"
    -- TODO:
    -- . the book specifies this arity as
    --   - `let(e1;x.e2)`
    --   - `(Exp, Exp.Exp)Exp`
    -- . is it known that the `x` binds `e1`?
    -- . where is `x` specified?
    , Operator "let"   (Arity ["Exp", Valence ["Exp"] "Exp"]) "definition"
    ])
  ]

getSort :: Matching a Sort
getSort = do
  MatchesEnv (SyntaxChart syntax) sort _ <- ask
  lift $ syntax ^? ix sort

completePattern :: Matching a Pattern
completePattern = do
  MatchesEnv _ sort _  <- ask
  Sort _vars operators <- getSort

  let mkPat (Operator opName arity _desc) = case arity of
        Arity valences -> PatternTm opName (const PatternAny <$> valences)
        -- TODO: PatternPrimTerm?
        External name  -> PatternPrimVal sort name

  pure $ PatternUnion $ fmap mkPat operators

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
        [ PatternPrimVal "Exp" "num"
        , PatternPrimVal "Exp" "str"
        , PatternTm "plus"  [PatternAny, PatternAny]
        , PatternTm "times" [PatternAny, PatternAny]
        , PatternTm "cat"   [PatternAny, PatternAny]
        , PatternTm "len"   [PatternAny]
        , PatternTm "let"   [PatternAny, PatternAny]
        ])
  ]

minus :: Pattern -> Pattern -> Matching a Pattern
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

minus (PatternUnion pats) x = do
  pats' <- traverse (`minus` x) pats
  pure $ PatternUnion $ filter (/= PatternEmpty) pats'

-- remove each pattern from pat one at a time
minus pat (PatternUnion pats) = foldlM minus pat pats

minus x@(PatternPrimVal sort1 name1) (PatternPrimVal sort2 name2) = pure $
  -- TODO: what if sorts not equal? is that an error?
  if sort1 == sort2 && name1 == name2 then PatternEmpty else x

minus x@(PatternPrimTerm sort1 _) (PatternPrimTerm sort2 _) = pure $
  if sort1 == sort2 then PatternEmpty else x

minus BindingPattern{} BindingPattern{} = error "TODO"

-- these all should return Nothing?
minus x@PatternTm{} PatternPrimVal{}     = pure x -- error $ "illegal: " ++ show (x, y)
minus x@PatternTm{} y@PatternPrimTerm{}    = error $ "illegal: " ++ show (x, y)
minus x@PatternTm{} BindingPattern{}     = pure x -- error $ "illegal: " ++ show (x, y)

minus x@PatternPrimVal{} PatternTm{}       = pure x
minus PatternPrimVal{} PatternPrimTerm{} = error "illegal"
minus x@PatternPrimVal{} BindingPattern{}  = pure x

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
      num = PatternPrimVal "Exp" "num"
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
       , expect $ runMatches eChart "Exp" (minus num num) == Just PatternEmpty
       , expect $
         runMatches eChart "Exp" (minus
           (PatternTm "plus"
             [ PatternPrimVal "Exp" "num"
             , x
             ])
           (PatternTm "plus"
             [ PatternPrimVal "Exp" "num"
             , y
             ]))
         ==
         Just PatternEmpty


       -- This is wrong:
       -- , expect $
       --   let env = MatchesEnv eChart "Exp" $ Map.fromList
       --         -- TODO: we should be able to do this without providing values
       --         [ ("x", Right (PrimValue 2))
       --         , ("y", Right (PrimValue 2))
       --         ]
       --   in (traceShowId $ flip runReaderT env (minus
       --        (PatternTm "plus" [x, y])
       --        (PatternTm "plus"
       --          [ PatternPrimVal "Exp" "num"
       --          , PatternPrimVal "Exp" "num"
       --          ])))
       --        ==
       --        Just PatternEmpty
       ]

patternCheck
  :: forall a. DenotationChart a
  -> Matching a (PatternCheckResult a)
patternCheck (DenotationChart chart) = do
  unmatched <- completePattern -- everything unmatched
  (overlaps, unmatched') <- mapAccumM
    (\unmatchedAcc (pat, _denotation) -> go unmatchedAcc pat)
    unmatched
    chart
  pure $ PatternCheckResult unmatched' overlaps

  -- go
  -- . takes the set of uncovered values
  -- . returns the (set of covered values, set of remaining uncovered values)
  where go :: Pattern -> Pattern -> Matching a ((Pattern, IsRedudant), Pattern)
        go unmatched pat = do
          pat' <- unmatched `minus` pat
              -- TODO: pretty sure we have to do some sort of noralization here
          let redundant = if pat == pat' then IsRedudant else IsntRedundant
          pure ((pat, redundant), pat')

findMatch
  :: forall a. Show a => DenotationChart a
  -> TmVal a
  -> Matching a (Subst a, Denotation a)
findMatch (DenotationChart pats) tm = do
  env <- ask
  let results = pats <&> \(pat, rhs) ->
        runReaderT (matches pat tm) env & _Just %~ (, rhs)

  lift $ getFirst $ fold $ fmap First results

-- judgements

data InOut = In | Out

data JudgementForm = JudgementForm
  { _judgementName  :: !Text                -- ^ name of the judgement
  , _judgementSlots :: ![(InOut, SortName)] -- ^ mode and sort of all slots
  }

data OperatorApplication = OperatorApplication
  { _operatorApName :: !Text            -- ^ operator name
  , _applicands     :: ![SaturatedTerm] -- ^ applicands
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
  = CbvForeignFrame
    { _frameName  :: !Text                       -- ^ name of this term
    , _frameVals  :: !(Seq (Value a))            -- ^ values before
    , _frameTerms :: ![Term a]                   -- ^ subterms after
    , _frameK     :: !(Seq (Value a) -> Value a) -- ^ what to do after
    }
  | CbvFrame
    -- { _cbvFrameVals  :: !(Map Text (Value a))
    -- , _cbvFrameTerms :: !(Map Text (Term a))
    { _cbvFrameArgName :: !Text
    , _cbvFrameBody    :: !(Term a)
    }
  | BindingFrame
    !(Map Text (TmVal a))

findBinding :: [StackFrame a] -> Text -> Maybe (TmVal a)
findBinding [] _ = Nothing
findBinding (BindingFrame bindings : stack) name
  = case bindings ^? ix name of
    Just (tmVal) -> Just tmVal
    Nothing      -> findBinding stack name
findBinding (_ : stack) name = findBinding stack name

frameVals :: [StackFrame a] -> Map Text (TmVal a)
frameVals = foldl
  (\sorts -> \case
    BindingFrame bindings -> bindings `Map.union` sorts
    _                     -> sorts)
  Map.empty

data StateStep a = StateStep
  { _stepFrames :: ![StackFrame a]
  , _stepFocus  :: !(TmVal a) -- ^ Either descending into term or ascending with value
  }

  | Errored !Text
  | Done !(Value a)

type State a = ListZipper (StateStep a)

type ListZipper a = Top :>> [a] :>> a

next :: State a -> State a
next state = case state ^. focus of
  Done{} -> state
  _      -> case state & rightward of
    Just state' -> state'
    Nothing     -> state

prev :: State a -> State a
prev state = case state & leftward of
  Just state' -> state'
  Nothing     -> state


makeLenses ''Sort
makeLenses ''Pattern
makeLenses ''Denotation
makeLenses ''Term
makeLenses ''Value
makeLenses ''PatternCheckResult
