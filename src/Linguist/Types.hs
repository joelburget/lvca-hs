{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
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
  , Sort(..)
  , SyntaxChart(..)
  , SortDef(..)
  , sortOperators
  , sortVariables
  , Operator(..)
  , Arity(..)
  , pattern ExternalArity
  , exampleArity
  , Valence(..)

  -- * Denotation charts
  -- | Denotational semantics definition.
  , DenotationChart(..)
  , pattern PatternAny
  , pattern PatternEmpty
  , Denotation(..)
  , fromSlots
  , toSlot
  , bodySlot
  , argSlots
  , Subst(..)

  -- * Terms / Values
  , Term(..)
  , TmShow(..)
  , subterms
  , termName

  -- * Patterns
  -- ** Pattern
  , Pattern(..)
  , patternPrimExternal
  , patternPrimName
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
  , completePattern
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
  , Focus(..)

  -- * Tests
  , toPatternTests
  ) where

import           Brick                     (Widget, str)
import           Control.Lens              hiding (op)
import           Control.Monad.Morph
import           Control.Monad.Reader
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
import           Data.Void                 (Void, absurd)
import           EasyTest                  hiding (pair)
import           GHC.Exts                  (IsList (..))
import           Linguist.Util


-- syntax charts

type SortName = Text
data Sort = SortAp !SortName ![Sort]
  deriving (Eq, Show)

instance IsString Sort where
  fromString s = SortAp (fromString s) []

-- | A syntax chart defines the abstract syntax of a language, specified by a
-- collection of operators and their arities. The abstract syntax provides a
-- systematic, unambiguous account of the hierarchical and binding structure of
-- the language.
--
-- @
-- Typ ::= num                numbers
--         str                strings
--
-- Exp ::= num[n]             numeral
--         str[s]             literal
--         plus(Exp; Exp)     addition
--         times(Exp; Exp)    multiplication
--         cat(Exp; Exp)      concatenation
--         len(Exp)           length
--         let(Exp; Exp.Exp)  definition
-- @
newtype SyntaxChart = SyntaxChart (Map SortName SortDef)
  deriving (Eq, Show)

-- | Sorts divide ASTs into syntactic categories. For example, programming
-- languages often have a syntactic distinction between expressions and
-- commands.
data SortDef = SortDef
  { _sortVariables :: ![Text]     -- ^ set of variables
  , _sortOperators :: ![Operator] -- ^ set of operators
  } deriving (Eq, Show)

-- | One of the fundamental constructions of a language. Operators are grouped
-- into sorts. Each operator has an /arity/, specifying its arguments.
data Operator = Operator
  { _operatorName  :: !Text  -- ^ operator name
  , _operatorArity :: !Arity -- ^ arity
  , _operatorDesc  :: !Text  -- ^ description
  } deriving (Eq, Show)

-- | An /arity/ specifies the sort of an operator and the number and valences
-- of its arguments.
--
-- eg @(Exp.Exp; Nat)Exp@.
--
-- To specify an arity, the resulting sort (the last @Exp@ above) is
-- unnecessary, since it's always clear from context. We also include
-- externals.
data Arity
  = Arity ![Valence]
  deriving (Eq, Show)

instance IsList Arity where
  type Item Arity  = Valence
  fromList         = Arity
  toList (Arity l) = l

-- | A /valence/ specifies the sort of an argument as well as the number and
-- sorts of the variables bound within it.
--
-- eg @Exp.Exp@.
data Valence = Valence
  { _valenceSorts  :: ![Sort] -- ^ the sorts of all bound variables
  , _valenceResult :: !Sort   -- ^ the resulting sort
  }
  | External !SortName
  deriving (Eq, Show)

instance IsString Valence where
  -- TODO: should we return an External when you pass "[nat]"?
  fromString = Valence [] . fromString

pattern ExternalArity :: SortName -> Arity
pattern ExternalArity name = Arity [External name]

-- | @exampleArity = 'Arity' ['Valence' [\"Exp\", \"Exp\"] \"Exp\"]@
exampleArity :: Arity
exampleArity = Arity [Valence ["Exp", "Exp"] "Exp"]


-- | An evaluated or unevaluated term
data Term a
  = Term
    { _termName :: !Text     -- ^ name of this term
    , _subterms :: ![Term a] -- ^ subterms
    }
  | Binding
    ![Text]
    !(Term a)
  | Var !Text
  | PrimValue !a
  deriving (Eq, Show)

class TmShow a where
  drawPrim :: a -> Widget ()

instance (TmShow a, TmShow b) => TmShow (Either a b) where
  drawPrim = either drawPrim drawPrim

instance TmShow Void where
  drawPrim = str . absurd

instance TmShow Int where
  drawPrim = str . show

instance TmShow Text where
  drawPrim = str . show

-- | A pattern matches a term
data Pattern
  -- | Matches the head of a term and any subpatterns
  = PatternTm !Text ![Pattern]

  -- TODO: should this exist?
  | BindingPattern ![Text] !Pattern

  -- | A variable pattern that matches everything, generating a substitution
  | PatternVar !(Maybe Text)

  -- | Matches 'PrimValue'
  | PatternPrimVal
    { _patternPrimExternal :: !SortName     -- ^ external name, eg "str"
    , _patternPrimName     :: !(Maybe Text) -- ^ variable name, eg "s_1"
    }

  -- | The union of patterns
  | PatternUnion ![Pattern]
  deriving (Eq, Show)

pattern PatternAny :: Pattern
pattern PatternAny = PatternVar Nothing

pattern PatternEmpty :: Pattern
pattern PatternEmpty = PatternUnion []

data Denotation a
  = Value
  | CallForeign !(Seq (Term a) -> Term a)
  | BindIn
    { _fromSlots :: ![(Text, Text)]
    , _toSlot    :: !Text
    }
  | Cbv
    { _bodySlot :: !Text
    , _argSlots :: ![Text]
    }
  | Cbn
    { _bodySlot :: !Text
    , _argSlots :: ![Text]
    }
  | Choose !Text

-- | Denotation charts
--
-- A denotation chart maps from patterns to their denotation. Patterns are
-- checked from top to bottom.
--
-- We check for completeness and reduncancy using a very similar algorithm to
-- Haskell's pattern match checks. These could also be compiled efficiently in
-- a similar way.
newtype DenotationChart a = DenotationChart [(Pattern, Denotation a)]

data Subst a = Subst
  { _assignments             :: !(Map Text (Term a))
  -- | pattern <-> term variable correspondences.
  , _variableCorrespondences :: ![(Text, Text)]
  } deriving (Eq, Show)

instance Semigroup (Subst a) where
  Subst x1 y1 <> Subst x2 y2 = Subst (x1 <> x2) (y1 <> y2)

instance Monoid (Subst a) where
  mempty = Subst mempty mempty

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
toPattern (Operator name (Arity valences) _desc)
  = PatternTm name $ const PatternAny <$> valences

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
    toPattern (Operator "num" (ExternalArity "nat") "numeral")
    ==
    PatternTm "num" [PatternAny]
  ]

data MatchesEnv a = MatchesEnv
  { _envChart :: !SyntaxChart
  -- | The language we're working in
  , _envSort  :: !SortName
  -- | The specific sort we're matching
  , _envVars  :: !(Map Text (Term a)) -- TODO: is this being used?
  }

makeLenses ''MatchesEnv

type Matching a = ReaderT (MatchesEnv a) Maybe

noMatch, emptyMatch :: Matching a (Subst a)
noMatch    = lift Nothing
emptyMatch = pure mempty

matches :: Pattern -> Term a -> Matching a (Subst a)
-- matches pat (Left (Return val))     = matches pat (Right val)
matches (PatternVar (Just name)) tm
  = pure $ Subst (Map.singleton name tm) []
matches (PatternVar Nothing)     _  = emptyMatch

matches pat (Var name) = do
  mTermVal <- view $ envVars . at name
  case mTermVal of
    Just termVal -> matches pat termVal
    Nothing      -> noMatch

matches (PatternTm name1 subpatterns) (Term name2 subterms) = do
  if name1 /= name2
  then noMatch
  else do
    mMatches <- sequence $ fmap sequence $
      pairWith matches subpatterns subterms
    mconcat <$> lift mMatches
-- TODO: write context of var names?
matches (PatternPrimVal _primExternal _varName) (PrimValue _)
  = emptyMatch
-- TODO: this piece must know the binding structure from the syntax chart
matches (BindingPattern lnames subpat) (Binding rnames subtm) = do
  subst <- Subst Map.empty <$> lift (pair lnames rnames)
  fmap (subst <>) $ matches subpat subtm
matches PatternAny _ = emptyMatch
matches (PatternUnion pats) tm = do
  env <- ask
  lift $ getFirst $ foldMap
    (\pat -> First $ runReaderT (pat `matches` tm) env)
    pats
matches _ _ = noMatch

runMatches :: SyntaxChart -> SortName -> Matching b a -> Maybe a
runMatches chart sort
  = flip runReaderT (MatchesEnv chart sort Map.empty)

getSort :: Matching a SortDef
getSort = do
  MatchesEnv (SyntaxChart syntax) sort _ <- ask
  lift $ syntax ^? ix sort

completePattern :: Matching a Pattern
completePattern = do
  SortDef _vars operators <- getSort

  pure $ PatternUnion $ operators <&>
    \(Operator opName (Arity valences) _desc) -> PatternTm opName $
      valences <&> \case
        External name -> PatternPrimVal name Nothing
        _valence      -> PatternAny

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

minus x@(PatternPrimVal external1 _) (PatternPrimVal external2 _)
  = pure $ if external1 == external2 then PatternEmpty else x

minus x@PatternTm{} PatternPrimVal{}      = pure x
minus x@PatternTm{} BindingPattern{}      = pure x

minus x@PatternPrimVal{} PatternTm{}      = pure x
minus x@PatternPrimVal{} BindingPattern{} = pure x

minus BindingPattern{} BindingPattern{}   = error "TODO"
minus x@BindingPattern{} PatternTm{}      = pure x
minus x@BindingPattern{} PatternPrimVal{} = pure x

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

  -- go:
  -- - takes the set of uncovered values
  -- - returns the (set of covered values, set of remaining uncovered values)
  where go :: Pattern -> Pattern -> Matching a ((Pattern, IsRedudant), Pattern)
        go unmatched pat = do
          pat' <- unmatched `minus` pat
          let redundant = if pat == pat' then IsRedudant else IsntRedundant
          pure ((pat, redundant), pat')

findMatch
  :: DenotationChart a
  -> Term a
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
    = pretty hd PP.<+> hsep (fmap pretty applicands)

instance Pretty SaturatedTerm where
  pretty = \case
    JVariable name -> pretty name
    Op opAp        -> parens $ pretty opAp

instance Pretty JudgementClause where
  pretty (JudgementClause hd applicands)
    = pretty hd PP.<+> hsep (fmap pretty applicands)

instance Pretty JudgementRule where
  pretty (JudgementRule assumptions conclusion) = vsep
    [ hsep $ punctuate comma $ fmap pretty assumptions
    , "------"
    , pretty conclusion
    ]

instance Pretty JudgementRules where
  pretty (JudgementRules rules) = vsep $ intersperse "" $ fmap pretty rules

instance Pretty SyntaxChart where
  pretty (SyntaxChart sorts) =
    let f (title, SortDef vars operators) = vsep
          [ let vars' = case vars of
                  [] -> ""
                  _  -> " " <> hsep (fmap pretty vars)
            in pretty title <> vars' <> " ::="
          , indent 2 $ vsep $ fmap pretty operators
          ]
    in vsep $ f <$> Map.toList sorts

instance Pretty Operator where
  pretty (Operator name arity _desc) = case arity of
    Arity [] -> pretty name
    _        -> pretty name <> pretty arity

instance Pretty Arity where
  pretty (Arity valences) = case valences of
    []                   -> mempty
    [valence@External{}] -> pretty valence
    _                    -> parens $ hsep $ punctuate semi $ fmap pretty valences

instance Pretty Sort where
  pretty (SortAp name args) = hsep $ pretty name : fmap pretty args

instance Pretty Valence where
  pretty (Valence boundVars result) = mconcat $
    punctuate dot (fmap pretty boundVars <> [pretty result])
  pretty (External name) = brackets (pretty name)


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


makeLenses ''SortDef
makeLenses ''Pattern
makeLenses ''Denotation
makeLenses ''Term
makeLenses ''PatternCheckResult
