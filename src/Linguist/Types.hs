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
  , externalName
  , _SortAp
  , _External
  , SyntaxChart(..)
  , SortDef(..)
  , sortOperators
  , sortVariables
  , sortSubst
  , Operator(..)
  , operatorName
  , operatorDesc
  , operatorArity
  , Arity(..)
  , valences
  , pattern ExternalArity
  , exampleArity
  , Valence(..)
  , valenceSubst
  , valenceSorts

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
  , VarBindingF(..)
  , varBindingP
  , PatVarF(..)
  , patVarP
  , MeaningOfF(..)
  , meaningP
  , TermF(..)
  , Term
  , _Term
  , _Binding
  , _Var
  , _PrimValue
  , subterms
  , termName
  , identify

  -- * Patterns
  -- ** Pattern
  , Pattern(..)
  , _PatternPrimVal
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
  , prop_serialise_identity
  ) where


import           Codec.Serialise
import           Codec.CBOR.Encoding       (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding       (decodeListLenOf, decodeWord)
import           Control.Lens              hiding (op)
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.ByteString           (ByteString)
import           Data.Foldable             (fold, foldlM, foldrM)
import           Data.List                 (intersperse, find)
import           Data.Maybe                (fromMaybe)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               (First (First, getFirst))
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Void                 (Void)
import           EasyTest
import           GHC.Exts                  (IsList (..))
import           Hedgehog                  (MonadGen, GenT, Property,
                                            property, forAll, (===))
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range

import           Linguist.FunctorUtil
import           Linguist.Util             as Util

-- syntax charts

type SortName = Text
data Sort
  = SortAp !SortName ![Sort]
  | External { _externalName :: !Text }
  deriving (Eq, Show)

instance IsString Sort where
  fromString s
    |  head s == '{'
    && last s == '}'
    = External $ Text.init $ fromString $ tail s
    | otherwise = SortAp (fromString s) []

sortSubst :: Map Text Sort -> Sort -> Sort
sortSubst varVals = \case
  e@External{} -> e
  SortAp name [] -> Map.findWithDefault (SortAp name []) name varVals
  SortAp name subSorts ->
    -- TODO: we need to cleanly separate variables from concrete sort names
    let subSorts' = sortSubst varVals <$> subSorts
    in SortAp name subSorts'

-- | A syntax chart defines the abstract syntax of a language, specified by a
-- collection of operators and their arities. The abstract syntax provides a
-- systematic, unambiguous account of the hierarchical and binding structure of
-- the language.
--
-- @
-- Typ ::= num                numbers
--         str                strings
--
-- Exp ::= {num}              numeral
--         {str}              literal
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
-- unnecessary, since it's always clear from context.
newtype Arity = Arity { _valences :: [Valence] }
  deriving (Eq, Show)

instance IsList Arity where
  type Item Arity  = Valence
  fromList         = Arity
  toList (Arity l) = l

-- | The arity of an operator holding only an external
pattern ExternalArity :: SortName -> Arity
pattern ExternalArity name = Arity [ Valence [] (External name) ]

-- | Apply a sort-substitution to a valence
valenceSubst :: Map Text Sort -> Valence -> Valence
valenceSubst m (Valence as b) = Valence (sortSubst m <$> as) (sortSubst m b)

-- | A /valence/ specifies the sort of an argument as well as the number and
-- sorts of the variables bound within it.
--
-- eg @Exp.Exp@.
data Valence = Valence
  { _valenceSorts  :: ![Sort] -- ^ the sorts of all bound variables
  , _valenceResult :: !Sort   -- ^ the resulting sort
  }
  deriving (Eq, Show)

-- | Traverse the sorts of both binders and body.
valenceSorts :: Traversal' Valence Sort
valenceSorts f (Valence sorts result)
  = Valence <$> traverse f sorts <*> f result

instance IsString Valence where
  fromString = Valence [] . fromString

-- | @exampleArity = 'Arity' ['Valence' [\"Exp\", \"Exp\"] \"Exp\"]@
exampleArity :: Arity
exampleArity = Arity [Valence ["Exp", "Exp"] "Exp"]

data VarBindingF f
  = BindingF ![Text] !f
  | VarF     !Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Show1 VarBindingF where
  liftShowsPrec showsf _ p tm = showParen (p > 10) $ case tm of
    BindingF nms f -> ss "BindingF " . showList nms . ss " " . showsf 11 f
    VarF mname     -> ss "VarF "  . showsPrec 11 mname
    where ss = showString

instance Eq1 VarBindingF where
  liftEq eq (BindingF n1 x) (BindingF n2 y) = n1 == n2 && eq x y
  liftEq _  (VarF x)        (VarF y)        = x == y
  liftEq _  _               _               = False

varBindingP
  :: Prism' (Term a)              (Fix f)
  -> Prism' (Term a) (VarBindingF (Fix f))
varBindingP p = prism' rtl ltr where
  rtl = \case
    BindingF names f -> Fix $ Binding names $ review p f
    VarF name        -> Fix $ Var name
  ltr = \case
    Fix (Binding names tm) -> BindingF names <$> preview p tm
    Fix (Var name)         -> Just $ VarF name
    _                      -> Nothing

data PatVarF f = PatVarF !(Maybe Text)
  deriving (Functor, Foldable, Traversable)

patVarP :: Prism' (Pattern a) (PatVarF (Fix f))
patVarP = prism' rtl ltr where
  rtl (PatVarF name) = PatternVar name
  ltr = \case
    PatternVar name -> Just $ PatVarF name
    _               -> Nothing

instance Show1 PatVarF where
  liftShowsPrec showsf _ p pat = showParen (p > 10) $ case pat of
    PatVarF mname -> showString "PatVarF " . showsPrec 11 mname

instance Eq1 PatVarF where
  liftEq _  (PatVarF x) (PatVarF y) = x == y
  liftEq _  _           _           = False

-- | An evaluated or unevaluated term
data TermF a term
  = Term
    { _termName :: !Text     -- ^ name of this term
    , _subterms :: ![term] -- ^ subterms
    }
  | Binding
    ![Text]
    !term
  | Var !Text
  | PrimValue !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Term a = Fix (TermF a)

instance Show1 (TermF a) where -- XXX
instance Eq1 (TermF a) where -- XXX

instance (Serialise a) => Serialise (Term a) where -- XXX
  encode = undefined
  decode = undefined

instance (Serialise a, Serialise term) => Serialise (TermF a term) where
  encode tm =
    let (tag, content) = case tm of
          Term      name  subtms -> (0, encode name  <> encode subtms)
          Binding   names body   -> (1, encode names <> encode body  )
          Var       name         -> (2, encode name                  )
          PrimValue a            -> (3, encode a                     )
    in encodeListLen 2 <> encodeWord tag <> content

  decode = do
    decodeListLenOf 2
    tag <- decodeWord
    case tag of
      0 -> Term      <$> decode <*> decode
      1 -> Binding   <$> decode <*> decode
      2 -> Var       <$> decode
      3 -> PrimValue <$> decode
      _ -> fail "invalid Term encoding"

prop_serialise_identity
  :: (Show a, Eq a, Serialise a)
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (GenT Identity a))
  -> Property
prop_serialise_identity chart sort aGen = property $ do
  tm <- forAll $ genTerm chart sort aGen
  -- (this is serialiseIdentity from Codec.Serialise.Properties)
  tm === (deserialise . serialise) tm

newtype Sha256 = Sha256 ByteString

-- | Content-identify a term via the sha-256 hash of its cbor serialization.
identify :: Serialise a => Term a -> Sha256
identify = Sha256 . SHA256.hashlazy . serialise

genName :: MonadGen m => m Text
genName = Text.cons
  <$> Gen.alpha
  <*> Gen.text (Range.exponential 0 500) Gen.alphaNum

genTerm
  :: forall m a.
     MonadGen m
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (m a))
  -> m (Term a)
genTerm _chart (External name) genPrim = case genPrim name of
  Nothing  -> Gen.discard
  Just gen -> Fix . PrimValue <$> gen
genTerm chart@(SyntaxChart chart') (SortAp sortHead sortArgs) genPrim = do
    let SortDef vars operators = chart' ^?! ix sortHead
        sortVarVals = Map.fromList $ zip vars sortArgs

        opNames = _operatorName <$> operators

        nonrec =
          [ do
               name <- genName
               if name `elem` opNames
               then Gen.discard
               else pure $ Fix $ Var name
          ]

        genArity :: [Valence] -> m [Term a]
        genArity = foldrM
          -- TODO: handle applied sorts
          (\valence valences' -> case valence of
            Valence binders sort -> do
              tm <- genTerm chart (sortSubst sortVarVals sort) genPrim
              case binders of
                [] -> pure $ tm : valences'
                _  -> do
                  names <- Gen.list (Range.singleton (length binders)) genName
                  pure $ Fix (Binding names tm) : valences'
          )
          []

        genTerm' :: Text -> [Valence] -> m (Term a)
        genTerm' name valences = Fix . Term name <$> genArity valences

        rec = operators <&> \(Operator name (Arity valences) _desc) ->
          genTerm' name valences

    Gen.sized $ \n ->
      if n <= 1 then
        Gen.choice nonrec
      else
        Gen.choice $ nonrec ++ fmap Gen.small rec


instance Pretty a => Pretty (Term a) where
  pretty (Fix tm) = case tm of
    Term name subtms ->
      pretty name <> parens (hsep $ punctuate semi $ fmap pretty subtms)
    Binding names tm ->
      hsep (punctuate dot (fmap pretty names)) <> dot PP.<+> pretty tm
    Var name    -> pretty name
    PrimValue a -> braces (pretty a)

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
  | PatternPrimVal !(Maybe a)
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
-- We check for completeness and redundancy using a very similar algorithm to
-- Haskell's pattern match checks. These could also be compiled efficiently in
-- a similar way.
newtype DenotationChart a b = DenotationChart [(Pattern a, Term b)]
  deriving Show

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
    Fix (Term name tms) -> Fix . Term name <$> ix k f tms
    _                   -> pure tm

-- TODO: do we have to normalize here?
isComplete :: Eq a => PatternCheckResult a -> Bool
isComplete (PatternCheckResult uc _) = uc == PatternUnion []

hasRedundantPat :: PatternCheckResult a -> Bool
hasRedundantPat (PatternCheckResult _ overlaps)
  = any ((== IsRedudant) . snd) overlaps

applySubst :: Show a => Subst a -> Term a -> Term a
applySubst subst@(Subst assignments correspondences) (Fix tm) = case tm of
  Term name subtms    -> Fix $ Term name (applySubst subst <$> subtms)
  Binding names subtm -> Fix $ Binding names (applySubst subst subtm)
  Var name            -> fromMaybe (Fix tm) $ do
    name' <- fst <$> find (\(_patName, tmName) -> name == tmName) correspondences
    assignments ^? ix name'
  _                   -> Fix tm

-- | Match any instance of this operator
toPattern :: Operator -> Pattern a
toPattern (Operator name (Arity valences) _desc)
  = PatternTm name $ valences <&> \case
      Valence [] External{} -> PatternPrimVal Nothing
      _valence              -> PatternAny

toPatternTests :: Test ()
toPatternTests = scope "toPattern" $
  let toPat :: Operator -> Pattern Void
      toPat = toPattern
  in tests
    [ expectEq
      (toPat (Operator "num" (Arity []) "numbers"))
      (PatternTm "num" [])
    , expectEq
      (toPat (Operator "plus"  (Arity ["Exp", "Exp"]) "addition"))
      (PatternTm "plus" [PatternAny, PatternAny])
    , expectEq
      (toPat (Operator "num" (ExternalArity "num") "numbers"))
      (PatternTm "num" [ PatternPrimVal Nothing ])
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

matches pat (Fix (Var name)) = do
  mTermVal <- view $ envVars . at name
  case mTermVal of
    Just termVal -> matches pat termVal
    Nothing      -> noMatch

matches (PatternTm name1 subpatterns) (Fix (Term name2 subterms))
  | name1 /= name2
  = noMatch
  | otherwise = do
    mMatches <- sequence $ fmap sequence $
      pairWith matches subpatterns subterms
    mconcat <$> lift mMatches

-- TODO: write context of var names?
matches (PatternPrimVal pVal) (Fix (PrimValue val))
  | pVal == Nothing
  = emptyMatch
  | pVal == Just val
  = emptyMatch
  | otherwise
  = noMatch

-- TODO: this piece must know the binding structure from the syntax chart
matches (BindingPattern lnames subpat) (Fix (Binding rnames subtm)) = do
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
runMatches chart sort = flip runReaderT (MatchesEnv chart sort Map.empty)

getSort :: Matching a SortDef
getSort = do
  MatchesEnv (SyntaxChart syntax) sort _ <- ask
  lift $ syntax ^? ix sort

completePattern :: Matching a (Pattern a)
completePattern = do
  SortDef _vars operators <- getSort
  pure $ PatternUnion $ toPattern <$> operators

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
    -- Elide parens if it's just an external
    ExternalArity name' -> pretty name <> braces (pretty name')
    _                   -> pretty name <> pretty arity

instance Pretty Arity where
  pretty (Arity valences) = case valences of
    [] -> mempty
    _  -> parens $ hsep $ punctuate semi $ fmap pretty valences

instance Pretty Sort where
  pretty (External name)    = braces $ pretty name
  pretty (SortAp name args) = hsep $ pretty name : fmap pretty args

instance Pretty Valence where
  pretty (Valence boundVars result) = mconcat $
    punctuate dot (fmap pretty boundVars <> [pretty result])


makeLenses ''Sort
makePrisms ''Sort
makeLenses ''SortDef
makeLenses ''Arity
makeWrapped ''Arity
makeLenses ''Pattern
makePrisms ''Pattern
makeLenses ''Operator
makeLenses ''TermF
makePrisms ''TermF
makeLenses ''PatternCheckResult

data MeaningOfF a
  = MeaningOf !Text
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Show1 MeaningOfF where
  liftShowsPrec _ _ p (MeaningOf name) = showParen (p > 10) $
    showString "MeaningOf " . showsPrec 11 name

instance Eq1 MeaningOfF where
  liftEq _  (MeaningOf  a1) (MeaningOf  a2) = a1 == a2

meaningP :: Prism' (Term (Either Text a)) (MeaningOfF (Fix f))
meaningP = prism' rtl ltr where
  rtl (MeaningOf name)
    = Fix $ Term "MeaningOf" [ review (_Fix . _PrimValue . _Left) name ]
  ltr = \case
    Fix (Term "MeaningOf" [name])
      -> MeaningOf <$> preview (_Fix . _PrimValue . _Left) name
    _ -> Nothing

data DenotationChart' (f :: * -> *) (g :: * -> *) = DenotationChart'
  [(Fix (PatVarF :+: f), Fix (VarBindingF :+: MeaningOfF :+: g))]
