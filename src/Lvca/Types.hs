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

module Lvca.Types
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
  -- ** Syntax components
  , SyntaxComponent(..)
  , componentChart
  , componentVars
  , SyntaxComponents(..)
  , syntaxComponents
  , componentMapping
  , findChartComponents

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
  , patVarP'
  , patVarP
  , MeaningOfF(..)
  , meaningOfP
  , TermF(..)
  , Term
  , _Term
  , _Binding
  , _Var
  , _PrimValue
  , subterms
  , termName
  , identify
  , TermRepresentable(..)
  , patAdaptor
  , termAdaptor

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

  -- * Other
  , prismSum
  , prismSum3
  , prismSum4
  ) where


import           Codec.Serialise
import           Codec.CBOR.Encoding       (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding       (decodeListLenOf, decodeWord)
import           Control.Lens              hiding (mapping, op, prism)
import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Bifunctor.TH               hiding (Options)
import           Data.ByteString           (ByteString)
import           Data.Data (Data)
import           Data.Eq.Deriving
import           Data.Foldable             (fold, foldlM)
import           Data.Graph                (stronglyConnCompR, SCC(..))
import           Data.List                 (intersperse, find)
import           Data.Maybe                (fromMaybe)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Matchable.TH
import           Data.Monoid               (First (First, getFirst))
import           Data.Proxy                (Proxy)
import           Data.String               (IsString (fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import           GHC.Exts                  (IsList (..))
import           GHC.Generics (Generic)
import           Text.Show.Deriving

import           Lvca.FunctorUtil
import           Lvca.Util             as Util

-- syntax charts

type SortName = Text
data Sort
  = SortAp !SortName ![Sort]
  | External { _externalName :: !Text }
  deriving (Eq, Show, Data)

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

-- | The list of sorts this sort (directly) depends on
sortDeps :: Sort -> [SortName]
sortDeps = \case
  SortAp name sorts -> name : concatMap sortDeps sorts
  External{}        -> []

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
  deriving (Eq, Show, Data)

data SyntaxComponent = SyntaxComponent
  { _componentChart :: !SyntaxChart
  -- ^ A sub-syntax chart, representing a strongly-connected subset of the
  -- original chart
  , _componentVars  :: ![[Text]]
  -- ^ All the variable names that occur as a parameter to any type in this
  -- component
  }

-- | It's sometimes useful to break a syntax chart into its connected
-- components, which contain data types dependent on each other, and which
-- become individual compilation units.
data SyntaxComponents = SyntaxComponents
  { _syntaxComponents :: ![SyntaxComponent]
  -- ^ List of sub-syntax charts, each representing a strongly-connected subset
  -- of the original chart
  , _componentMapping :: !(Map SortName SyntaxComponent)
  -- ^ Map from the originally defined sorts to the component they occur in
  }

-- | Find the strongly connected components / compilation units of a family of
-- data types.
findChartComponents :: SyntaxChart -> SyntaxComponents
findChartComponents (SyntaxChart sorts) =
  let sortsWithDeps :: [(SortDef, SortName, [SortName])]
      sortsWithDeps = Map.toList sorts <&> \(name, sortDef@(SortDef _ ops)) ->
        (sortDef,name,) $ concat3 $
          ops <&> \(Operator _name (Arity valences) _desc) ->
            valences <&> \(Valence vSorts result) ->
              fmap sortDeps $ result : vSorts

      components :: [SCC (SortDef, SortName, [SortName])]
      components = stronglyConnCompR sortsWithDeps

      (components', mappings) = unzip $ components <&> \scc ->
        let preChart :: [(SortName, SortDef)]
            (preChart, names) = case scc of
              AcyclicSCC (sortDef, name, _) ->
                ( [(name, sortDef)]
                , [name]
                )
              CyclicSCC vertices ->
                ( vertices <&> (\(sortDef, name, _) -> (name, sortDef))
                , vertices ^.. traverse . _2
                )

            -- Collect all the variable names that occur in any sorts
            vars = preChart <&> \(_, SortDef sortVars _) -> sortVars

            component = SyntaxComponent
              (SyntaxChart (Map.fromList preChart))
              vars
        in (component, Map.fromList $ zip names $ repeat component)
  in SyntaxComponents components' (Map.unions mappings)

-- | Sorts divide ASTs into syntactic categories. For example, programming
-- languages often have a syntactic distinction between expressions and
-- commands.
data SortDef = SortDef
  { _sortVariables :: ![Text]     -- ^ set of variables
  , _sortOperators :: ![Operator] -- ^ set of operators
  } deriving (Eq, Show, Data)

-- | One of the fundamental constructions of a language. Operators are grouped
-- into sorts. Each operator has an /arity/, specifying its arguments.
data Operator = Operator
  { _operatorName  :: !Text  -- ^ operator name
  , _operatorArity :: !Arity -- ^ arity
  , _operatorDesc  :: !Text  -- ^ description
  } deriving (Eq, Show, Data)

-- | An /arity/ specifies the sort of an operator and the number and valences
-- of its arguments.
--
-- eg @(Exp.Exp; Nat)Exp@.
--
-- To specify an arity, the resulting sort (the last @Exp@ above) is
-- unnecessary, since it's always clear from context.
newtype Arity = Arity { _valences :: [Valence] }
  deriving (Eq, Show, Data)

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
  } deriving (Eq, Show, Data)

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

patVarP' :: Prism' (Pattern a) (PatVarF (Fix f))
patVarP' = prism' rtl ltr where
  rtl (PatVarF name) = PatternVar name
  ltr = \case
    PatternVar name -> Just $ PatVarF name
    _               -> Nothing

patVarP :: TermRepresentable f => Prism' (Pattern a) (Fix (PatVarF :+: f a))
patVarP = sumPrisms patVarP' (mkPatP patVarP)

instance Show1 PatVarF where
  liftShowsPrec _ _ p pat = showParen (p > 10) $ case pat of
    PatVarF mname -> showString "PatVarF " . showsPrec 11 mname

instance Eq1 PatVarF where
  liftEq _  (PatVarF x) (PatVarF y) = x == y

-- | An evaluated or unevaluated term
data TermF a term
  = Term
    { _termName :: !Text   -- ^ name of this term
    , _subterms :: ![term] -- ^ subterms
    }
  | Binding
    ![Text]
    !term
  | Var !Text
  | PrimValue !a
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

type Term a = Fix (TermF a)

class TermRepresentable f where
  syntaxOf :: Proxy f -> SyntaxChart -- TODO: should we get rid of this?

  -- TODO: could we make both of these from one?
  mkTermP
    :: Prism' (Term a)      (Fix f')
    -> Prism' (Term a) (f a (Fix f'))
  mkPatP
    :: Prism' (Pattern a)      (Fix f')
    -> Prism' (Pattern a) (f a (Fix f'))

instance TermRepresentable TermF where
  syntaxOf = undefined
  mkTermP p = prism' rtl ltr where
    rtl = Fix . \case
      Term name subtms -> Term name $ review p <$> subtms
      Binding names tm -> Binding names $ review p tm
      Var name         -> Var name
      PrimValue a      -> PrimValue a
    ltr (Fix tm) = case tm of
      Term name subtms -> Term name <$> traverse (preview p) subtms
      Binding names tm -> Binding names <$> preview p tm
      Var name         -> pure $ Var name
      PrimValue a      -> pure $ PrimValue a
  mkPatP p = prism' rtl ltr where
    rtl tm = case tm of
      Term name subtms -> PatternTm name $ review p <$> subtms
      Binding names tm -> error "XXX"
      Var name         -> PatternVar (Just name)
      PrimValue a      -> PatternPrimVal (Just a)
    ltr = \case
      PatternTm name subpats  -> Term name <$> traverse (preview p) subpats
      PatternVar (Just name)  -> Just $ Var name
      PatternVar Nothing      -> error "TODO"
      PatternPrimVal (Just a) -> pure $ PrimValue a
      PatternPrimVal Nothing  -> error "TODO"
      PatternUnion{}          -> Nothing

class HasPrism a b where
  prism :: Prism' a b

instance HasPrism (Term a) (Term a) where
  prism = id

instance HasPrism (Pattern a) (Pattern a) where
  prism = id

instance HasPrism a b => HasPrism (Pattern a) (Pattern b) where
  prism = patAdaptor prism

patAdaptor :: Prism' a b -> Prism' (Pattern a) (Pattern b)
patAdaptor p = prism' rtl ltr where
  rtl = \case
    PatternTm name subpats  -> PatternTm name $ rtl <$> subpats
    PatternVar v            -> PatternVar v
    PatternPrimVal Nothing  -> PatternPrimVal Nothing
    PatternPrimVal (Just a) -> PatternPrimVal $ Just $ review p a
    PatternUnion subpats    -> PatternUnion $ rtl <$> subpats
  ltr = \case
    PatternTm name subpats  -> PatternTm name <$> traverse ltr subpats
    PatternVar v            -> pure $ PatternVar v
    PatternPrimVal Nothing  -> pure $ PatternPrimVal Nothing
    PatternPrimVal (Just a) -> PatternPrimVal . Just <$> preview p a
    PatternUnion subpats    -> PatternUnion <$> traverse ltr subpats

prismSum ::
  ( HasPrism a (f1 (Fix (f1 :+: f2)))
  , HasPrism a (f2 (Fix (f1 :+: f2)))
  ) => Prism' a (Fix ((f1 :+: f2)))
prismSum = sumPrisms prism prism

prismSum3 ::
  ( HasPrism a (f1 (Fix (f1 :+: f2 :+: f3)))
  , HasPrism a (f2 (Fix (f1 :+: f2 :+: f3)))
  , HasPrism a (f3 (Fix (f1 :+: f2 :+: f3)))
  ) => Prism' a (Fix ((f1 :+: f2 :+: f3)))
prismSum3 = sumPrisms3 prism prism prism

prismSum4 ::
  ( HasPrism a (f1 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  , HasPrism a (f2 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  , HasPrism a (f3 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  , HasPrism a (f4 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  ) => Prism' a (Fix ((f1 :+: f2 :+: f3 :+: f4)))
prismSum4 = sumPrisms4 prism prism prism prism

instance HasPrism a b => HasPrism (Term a) (Term b) where
  prism = termAdaptor prism

termAdaptor :: Prism' a b -> Prism' (Term a) (Term b)
termAdaptor p = prism' rtl ltr where
  rtl (Fix tm) = Fix $ case tm of
    Term name subtms -> Term name $ rtl <$> subtms
    Binding name tm' -> Binding name $ rtl tm'
    Var v            -> Var v
    PrimValue a      -> PrimValue $ review p a
  ltr (Fix tm) = fmap Fix $ case tm of
    Term name subtms -> Term name <$> traverse ltr subtms
    Binding name tm' -> Binding name <$> ltr tm'
    Var v            -> Just $ Var v
    PrimValue a      -> PrimValue <$> preview p a

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

instance (Serialise a) => Serialise (Term a) where
  encode (Fix term) = encode term
  decode            = Fix <$> decode

newtype Sha256 = Sha256 ByteString

-- | Content-identify a term via the sha-256 hash of its cbor serialization.
identify :: Serialise a => Term a -> Sha256
identify = Sha256 . SHA256.hashlazy . serialise

instance Pretty a => Pretty (Term a) where
  pretty (Fix tm) = case tm of
    Term name subtms ->
      pretty name <> parens (hsep $ punctuate semi $ fmap pretty subtms)
    Binding names tm' ->
      hsep (punctuate dot (fmap pretty names)) <> dot PP.<+> pretty tm'
    Var name    -> pretty name
    PrimValue a -> braces (pretty a)

instance Pretty a => Pretty (Pattern a) where
  pretty = \case
    PatternTm name subpats ->
      pretty name <> parens (hsep $ punctuate semi $ fmap pretty subpats)
    PatternVar Nothing -> "_"
    PatternVar (Just name) -> pretty name
    PatternPrimVal a -> braces (pretty a)
    PatternUnion pats -> group $ encloseSep
      (flatAlt "( " "(")
      (flatAlt " )" ")")
      " | "
      (pretty <$> pats)

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

deriveShow1 ''TermF
deriveEq1   ''TermF
deriveShow2 ''TermF
deriveEq2   ''TermF

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

instance (Pretty a, Pretty b) => Pretty (DenotationChart a b) where
  pretty (DenotationChart rows) = vsep $ rows <&> \(pat, tm) ->
    "[[ " <> pretty pat <> " ]] = " <> pretty tm

pattern (:->) :: a -> b -> (a, b)
pattern a :-> b = (a, b)

(<->) :: a -> b -> (a, b)
a <-> b = (a, b)

newtype Subst a = Subst { _assignments :: (Map Text (Term a)) }
  deriving (Eq, Show, Semigroup, Monoid)

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
applySubst subst@(Subst assignments) (Fix tm) = case tm of
  Term name subtms    -> Fix $ Term name (applySubst subst <$> subtms)
  Binding names subtm -> Fix $ Binding names (applySubst subst subtm)
  Var name            -> fromMaybe (Fix tm) $ do
    error "TODO"
    -- name' <- fst <$> find (\(_patName, tmName) -> name == tmName) correspondences
    -- assignments ^? ix name'
  _                   -> Fix tm

-- | Match any instance of this operator
toPattern :: Operator -> Pattern a
toPattern (Operator name (Arity valences) _desc)
  = PatternTm name $ valences <&> \case
      Valence [] External{} -> PatternPrimVal Nothing
      _valence              -> PatternAny

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
  = pure $ Subst $ Map.singleton name tm
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

minus x@PatternTm{} PatternPrimVal{}      = pure x

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

makeLenses ''SyntaxComponent
makeLenses ''SyntaxComponents
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
deriveBifunctor ''TermF
deriveBifoldable        ''TermF
deriveBitraversable     ''TermF
deriveBimatchable       ''TermF
makeLenses ''PatternCheckResult

data MeaningOfF a
  = MeaningOf !Text
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Show1 MeaningOfF where
  liftShowsPrec _ _ p (MeaningOf name) = showParen (p > 10) $
    showString "MeaningOf " . showsPrec 11 name

instance Eq1 MeaningOfF where
  liftEq _  (MeaningOf  a1) (MeaningOf  a2) = a1 == a2

meaningOfP :: Prism' (Term a) Text -> Prism' (Term a) (MeaningOfF (Fix f))
meaningOfP textP = prism' rtl ltr where
  rtl (MeaningOf name)
    = Fix $ Term "MeaningOf" [ review textP name ]
  ltr = \case
    Fix (Term "MeaningOf" [name])
      -> MeaningOf <$> preview textP name
    _ -> Nothing

data DenotationChart' (f :: * -> *) (g :: * -> *) = DenotationChart'
  [(Fix (PatVarF :+: f), Fix (VarBindingF :+: MeaningOfF :+: g))]
  deriving Show
