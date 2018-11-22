{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TemplateHaskell     #-}
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
  , DenotationChart'(..)
  , pattern (:->)
  , (<->)
  , pattern PatternAny
  , pattern PatternEmpty
  , Subst(..)

  -- * Terms / Values
  , Term(..)
  , _Term
  , _Binding
  , _Var
  , _PrimValue
  , subterms
  , termName

  -- * Patterns
  -- ** Pattern
  , Pattern(..)
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
  , Matching
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

  -- * Tests
  , toPatternTests
  , genTerm
  ) where


import           Control.Lens              hiding (op)
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Data.Foldable             (fold, foldlM)
import           Data.List                 (intersperse, find)
import Data.Maybe (fromMaybe)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               (First (First, getFirst))
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Void                 (Void)
import           EasyTest
import           GHC.Exts                  (IsList (..))
import           Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Linguist.FunctorUtil
import           Linguist.Util as Util


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

-- TODO: alphaNum after first char
genName :: MonadGen m => m Text
genName = Gen.text (Range.exponential 1 500) Gen.alpha

genTerm :: forall m a. MonadGen m => Maybe (m a) -> m (Term a)
genTerm aGen =
  let nonRecs = concat
        [ case aGen of
            Nothing    -> []
            Just aGen' -> [ PrimValue <$> aGen' ]
        , [ Var  <$> genName ]
        , [ Term <$> genName <*> pure [] ]
        ]
      genTerm' = genTerm aGen
      recs =
        [ Gen.subtermM genTerm' (\x -> Term <$> genName <*> pure [ x ])
        , Gen.subtermM2 genTerm' genTerm' (\x y -> Term <$> genName <*> pure [ x, y ])
        , Gen.subtermM3 genTerm' genTerm' genTerm' (\x y z -> Term <$> genName <*> pure [ x, y, z ])
        -- Term <$> genName <*> Gen.list (Range.exponential 1 100)

        , Gen.subtermM genTerm'
          (\x -> Binding <$> Gen.list (Range.exponential 1 100) genName <*> pure x)
        ]
  in Gen.recursive Gen.choice nonRecs recs

instance Pretty a => Pretty (Term a) where
  pretty = \case
    Term name subtms ->
      pretty name <> parens (hsep $ punctuate semi $ fmap pretty subtms)
    Binding names tm ->
      hsep (punctuate dot (fmap pretty names)) <> dot PP.<+> pretty tm
    Var name         -> pretty name
    PrimValue a      -> pretty a

-- | A pattern matches a term.
--
-- In fact, patterns and terms have almost exactly the same form. Differences:
-- * Patterns add unions so that multiple things can match for the same
--   right-hand side
-- * In addition to variables, patterns can match wildcards, which don't bind
--   any variable.
data Pattern a
  -- | Matches the head of a term and any subpatterns
  = PatternTm !Text ![Pattern a]
  -- TODO: should this exist?
  | BindingPattern ![Text] !(Pattern a)
  -- TODO: Add non-binding, yet named variables, eg _foo
  -- | A variable pattern that matches everything, generating a substitution
  | PatternVar !(Maybe Text)
  | PatternPrimVal a
  -- | The union of patterns
  | PatternUnion ![Pattern a]
  deriving (Eq, Show)

pattern PatternAny :: Pattern a
pattern PatternAny = PatternVar Nothing

pattern PatternEmpty :: Pattern a
pattern PatternEmpty = PatternUnion []


-- | Denotation charts
--
-- A denotation chart maps from patterns to their denotation. Patterns are
-- checked from top to bottom.
--
-- We check for completeness and reduncancy using a very similar algorithm to
-- Haskell's pattern match checks. These could also be compiled efficiently in
-- a similar way.
newtype DenotationChart a b = DenotationChart [(Pattern a, Term b)]
  deriving Show

data DenotationChart' (f :: * -> *) (g :: * -> *)
  = DenotationChart' [(Fix f, Free g Text)]

pattern (:->) :: a -> b -> (a, b)
pattern a :-> b = (a, b)

(<->) :: a -> b -> (a, b)
a <-> b = (a, b)

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
  { _uncovered   :: !(Pattern a)               -- ^ uncovered patterns
  , _overlapping :: ![(Pattern a, IsRedudant)] -- ^ overlapping part, redundant?
  }

type instance Index   (Term a) = Int
type instance IxValue (Term a) = Term a
instance Ixed (Term a) where
  ix k f tm = case tm of
    Term name tms -> Term name <$> ix k f tms
    _             -> pure tm

isComplete :: Eq a => PatternCheckResult a -> Bool
isComplete (PatternCheckResult uc _) = uc == PatternUnion []

hasRedundantPat :: PatternCheckResult a -> Bool
hasRedundantPat (PatternCheckResult _ overlaps)
  = any ((== IsRedudant) . snd) overlaps

applySubst :: Show a => Subst a -> Term a -> Term a
applySubst subst@(Subst assignments correspondences) tm = case tm of
  Term name subtms    -> Term name (applySubst subst <$> subtms)
  Binding names subtm -> Binding names (applySubst subst subtm)
  Var name            -> fromMaybe tm $ do
    name' <- fst <$> find (\(_patName, tmName) -> name == tmName) correspondences
    assignments ^? ix name'
  _                   -> tm

toPattern :: Operator -> Pattern a
toPattern (Operator name (Arity valences) _desc)
  = PatternTm name $ const PatternAny <$> valences

toPatternTests :: Test ()
toPatternTests = scope "toPattern" $
  let toPat :: Operator -> Pattern Void
      toPat = toPattern
  in tests
    [ expect $
      toPat (Operator "num" (Arity []) "numbers")
      ==
      PatternTm "num" []
    , expect $
      toPat (Operator "plus"  (Arity ["Exp", "Exp"]) "addition")
      ==
      PatternTm "plus" [PatternAny, PatternAny]
    , expect $
      toPat (Operator "num" (ExternalArity "nat") "numeral")
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

matches :: (Show a, Eq a) => Pattern a -> Term a -> Matching a (Subst a)
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
matches (PatternPrimVal pVal) (PrimValue val)
  | pVal == val = emptyMatch
  | otherwise   = noMatch
-- TODO: this piece must know the binding structure from the syntax chart
matches (BindingPattern lnames subpat) (Binding rnames subtm) = do
  subst <- Subst Map.empty <$> lift (Util.pair lnames rnames)
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

completePattern :: Matching a (Pattern a)
completePattern = do
  SortDef _vars operators <- getSort

  pure $ PatternUnion $ operators <&>
    \(Operator opName (Arity valences) _desc) -> PatternTm opName $
      valences <&> \case
        -- This requires the convention that externals are always stored by
        -- themselves in a term named after their type. Perhaps the match
        -- should just be PatternAny?
        External name -> PatternTm name [PatternAny]
        _valence      -> PatternAny

minus :: Eq a => Pattern a -> Pattern a -> Matching a (Pattern a)
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

minus x@(PatternPrimVal a) (PatternPrimVal b)
  = pure $ if a == b then PatternEmpty else x
minus x@PatternPrimVal{} _ = pure x

minus x@PatternTm{} BindingPattern{}      = pure x
minus x@PatternTm{} PatternPrimVal{}      = pure x

minus BindingPattern{} BindingPattern{}   = error "TODO"
minus x@BindingPattern{} PatternTm{}      = pure x
minus x@BindingPattern{} PatternPrimVal{} = pure x

patternCheck
  :: forall a b.
     Eq a
  => DenotationChart a b
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
  where go
          :: Pattern a
          -> Pattern a
          -> Matching a ((Pattern a, IsRedudant), Pattern a)
        go unmatched pat = do
          pat' <- unmatched `minus` pat
          let redundant = if pat == pat' then IsRedudant else IsntRedundant
          pure ((pat, redundant), pat')

findMatch
  :: (Eq a, Show a, Show b)
  => DenotationChart a b
  -> Term a
  -> Matching a (Subst a, Term b)
findMatch (DenotationChart pats) tm = do
  env <- ask
  let results = pats <&> \(pat, rhs) ->
        runReaderT (matches pat tm) env & _Just %~ (, rhs)

  lift $ getFirst $ fold $ fmap First results

-- judgements

data InOut = JIn | JOut

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


makeLenses ''SortDef
makeLenses ''Pattern
makeLenses ''Term
makePrisms ''Term
makeLenses ''PatternCheckResult
