{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
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
  ( module Lvca.Judgements
    -- * Abstract syntax charts
    -- | Syntax definition.
  , Sort(..)
  , externalName
  , _SortAp
  , _External
  , SyntaxChart(..)
  , syntaxChartContents
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

  -- | Concrete syntax charts
  , MixfixDirective(..)
  , (>>:)
  , nil
  , space
  , OperatorDirective(..)
  , ConcreteSyntaxRule(..)
  , Fixity(..)
  , ConcreteSyntax(..)
  , mkConcreteSyntax

  -- * Denotation charts
  -- | Denotational semantics definition.
  , DenotationChart(..)
  , pattern (:->)
  , (<->)
  , pattern PatternAny
  , pattern PatternEmpty
  , Subst(..)

  -- * Terms / Values
  , Term(..)
  -- ** Term patterns
  , pattern BinaryTerm
  -- ** Term prisms / traversals
  , _Term
  , _Binding
  , _Var
  , _PrimValue
  , subterms
  , termName
  , identify
  -- ** TermRepresentable / PatternRepresentable
  , patAdaptor
  , termAdaptor

  -- * Patterns
  -- ** Pattern
  , Pattern(..)
  , _PatternTm
  , _PatternVar
  , _PatternPrimVal
  , _PatternUnion
  -- ** PatternCheckResult
  , PatternCheckResult(..)
  , IsRedudant(..)
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
  ) where

import           Codec.CBOR.Decoding       (decodeListLenOf, decodeWord)
import           Codec.CBOR.Encoding       (encodeListLen, encodeWord)
import           Codec.Serialise
import           Control.Lens              hiding (Empty, mapping, op, prism)
import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Aeson
  (FromJSON(..), ToJSON(..), Value(..), withArray)
import           Data.ByteString           (ByteString)
import           Data.Data                 (Data)
import           Data.Eq.Deriving
import           Data.Foldable             (fold, foldlM)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Matchable.TH
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (First(First, getFirst))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.String               (IsString(fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc hiding (space, (<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Vector               as Vector
import           GHC.Exts                  (IsList(..))
import           GHC.Generics              (Generic)
import           Prelude                   hiding (lookup)
import           Text.Show.Deriving

import           Lvca.Judgements
import           Lvca.Util                 as Util

pattern (:->) :: a -> b -> (a, b)
pattern a :-> b = (a, b)

(<->) :: a -> b -> (a, b)
a <-> b = (a, b)

-- syntax charts

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
newtype SyntaxChart = SyntaxChart
  { _syntaxChartContents :: Map SortName SortDef }
  deriving (Eq, Show, Data)

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
  { _operatorName  :: !OperatorName -- ^ operator name
  , _operatorArity :: !Arity        -- ^ arity
  , _operatorDesc  :: !Text         -- ^ description
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

-- | The arity of an operator holding only an external.
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

-- | Parsing / pretty-printing directive
data MixfixDirective
  = Literal !Text
  | Sequence !MixfixDirective !MixfixDirective
  | Line
  | Nest !Int !MixfixDirective
  | Group !MixfixDirective
  | (:<+) !MixfixDirective !MixfixDirective
  | SubTerm !Text
  deriving (Eq, Show)

instance Pretty MixfixDirective where
  pretty = \case
    Literal str -> dquotes $ pretty str
    Sequence a b -> hsep [pretty a, pretty b]
    Line -> hardline
    Nest _ _ -> "TODO Nest"
    Group d -> "group(" <> pretty d <> ")"
    _ :<+ _ -> "TODO :<+"
    SubTerm name -> pretty name

infixr 5 >>:
(>>:) :: MixfixDirective -> MixfixDirective -> MixfixDirective
a >>: b = Sequence a b

nil, space :: MixfixDirective
nil   = Literal "" :<+ space

space = Literal " " :<+ Literal "\n" -- line instead of \n?

instance IsString MixfixDirective where
  fromString = Literal . fromString

-- TODO:
-- + block model / smart spacing

-- | Whether a binary operator is left-, right-, or non-associative
data Fixity
  = Infixl -- ^ An operator associating to the left:
           -- (@x + y + z ~~ (x + y) + z@)
  | Infixr -- ^ An operator associating to the right:
           -- (@x $ y $ z ~~ x $ (y $ z)@)
  | Infix  -- ^ A non-associative operator
  deriving (Eq, Show)

instance Pretty Fixity where
  pretty = \case
    Infixl -> "infixl"
    Infixr -> "infixr"
    Infix  -> "infix"

data OperatorDirective
  = InfixDirective  !Text !Fixity
  | MixfixDirective !MixfixDirective
  deriving (Eq, Show)

instance Pretty OperatorDirective where
  pretty = \case
    InfixDirective name fixity ->
      hsep [pretty fixity, "x", dquotes (pretty name), "y"]
    MixfixDirective dir -> pretty dir

data ConcreteSyntaxRule = ConcreteSyntaxRule
  { _csOperatorName    :: !OperatorName
  , _boundVars         :: ![Text]
  , _operatorDirective :: !OperatorDirective
  } deriving (Eq, Show)

-- | A concrete syntax chart specifies how to parse and pretty-print a language
--
-- Each level of the chart corresponds to a precendence level, starting with
-- the highest precendence and droping to the lowest. Example:
--
-- > ConcreteSyntax
-- >   [ [ "Z"   :-> "Z" ]
-- >   , [ "S"   :-> "S" >>: space >>: Arith ]
-- >   , [ "Mul" :-> InfixDirective "*" Infixl ]
-- >   , [ "Add" :-> InfixDirective "+" Infixl
-- >     , "Sub" :-> InfixDirective "-" Infixl
-- >     ]
-- >   ]
newtype ConcreteSyntax = ConcreteSyntax (Seq [ConcreteSyntaxRule])
  deriving (Eq, Show)
-- TODO: randomly generate syntax descriptions and test that they round-trip
-- parsing and pretty-printing

instance Pretty ConcreteSyntax where
  pretty (ConcreteSyntax precLevels) = vsep $ "concrete syntax:" : prettyLevels
    where prettyLevels = toList precLevels <&> \decls -> indent 2 $
            ("-" PP.<+>) $ align $ vsep $ decls <&>
              \(ConcreteSyntaxRule op vars directive) -> hsep
                [ pretty op <> encloseSep "(" ")" "; " (map pretty vars)
                , "~"
                , pretty directive
                ]

mkConcreteSyntax :: [[ConcreteSyntaxRule]] -> ConcreteSyntax
mkConcreteSyntax = ConcreteSyntax . Seq.fromList

-- | An evaluated or unevaluated term
data Term a
  = Term
    { _termName :: !Text   -- ^ name of this term
    , _subterms :: ![Term a] -- ^ subterms
    }
  | Binding
    ![Text]
    !(Term a)
  | Var !Text
  | PrimValue !a
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

arr :: [Value] -> Value
arr = Array . Vector.fromList

instance ToJSON a => ToJSON (Term a) where
  toJSON = \case
    Term name subtms   -> arr [String "t", toJSON name, toJSON subtms]
    Binding vars subtm -> arr [String "b", toJSON vars, toJSON subtm]
    Var name           -> arr [String "v", toJSON name]
    PrimValue a        -> arr [String "p", toJSON a]

instance FromJSON a => FromJSON (Term a) where
  parseJSON = withArray "Term" $ \v -> case Vector.toList v of
    [String "t", a, b] -> Term      <$> parseJSON a <*> parseJSON b
    [String "b", a, b] -> Binding   <$> parseJSON a <*> parseJSON b
    [String "v", a   ] -> Var       <$> parseJSON a
    [String "p", a   ] -> PrimValue <$> parseJSON a
    -- TODO: better message
    _                  -> fail "unexpected JSON format for Term"

pattern BinaryTerm :: Text -> Term a -> Term a -> Term a
pattern BinaryTerm name x y = Term name [x, y]

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

termAdaptor :: Prism' a b -> Prism' (Term a) (Term b)
termAdaptor p = prism' rtl ltr where
  rtl tm = case tm of
    Term name subtms -> Term name $ rtl <$> subtms
    Binding name tm' -> Binding name $ rtl tm'
    Var v            -> Var v
    PrimValue a      -> PrimValue $ review p a
  ltr tm = case tm of
    Term name subtms -> Term name <$> traverse ltr subtms
    Binding name tm' -> Binding name <$> ltr tm'
    Var v            -> Just $ Var v
    PrimValue a      -> PrimValue <$> preview p a

instance Serialise a => Serialise (Term a) where
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

newtype Sha256 = Sha256 ByteString

-- | Content-identify a term via the sha-256 hash of its cbor serialization.
identify :: Serialise a => Term a -> Sha256
identify = Sha256 . SHA256.hashlazy . serialise

instance Pretty a => Pretty (Term a) where
  pretty = \case
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
--
--   * Patterns add unions so that multiple things can match for the same
--     right-hand side
--   * In addition to variables, patterns can match wildcards, which don't bind
--     any variable.
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

deriveShow1 ''Term
deriveEq1   ''Term
deriveShow1 ''Pattern
deriveEq1   ''Pattern

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

newtype Subst a = Subst { _assignments :: Map Text (Term a) }
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
    Term name tms -> Term name <$> ix k f tms
    _             -> pure tm

-- TODO: do we have to normalize here?
isComplete :: Eq a => PatternCheckResult a -> Bool
isComplete (PatternCheckResult uc _) = uc == PatternUnion []

hasRedundantPat :: PatternCheckResult a -> Bool
hasRedundantPat (PatternCheckResult _ overlaps)
  = any ((== IsRedudant) . snd) overlaps

applySubst :: Show a => Subst a -> Term a -> Term a
applySubst subst@(Subst assignments) tm = case tm of
  Term name subtms    -> Term name (applySubst subst <$> subtms)
  Binding names subtm -> Binding names (applySubst subst subtm)
  Var name            -> fromMaybe tm $ assignments ^? ix name
  _                   -> tm

-- | Match any instance of this operator
toPattern :: Operator -> Pattern a
toPattern (Operator name (Arity valences) _)
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
matches (PatternVar (Just name)) tm
  = pure $ Subst $ Map.singleton name tm
matches (PatternVar Nothing)     _
  = emptyMatch

matches pat (Var name)
  = do mTermVal <- view $ envVars . at name
       case mTermVal of
         Just termVal -> matches pat termVal
         Nothing      -> noMatch

matches (PatternTm name1 subpatterns) (Term name2 subterms)
  | name1 /= name2
  = noMatch
  | otherwise = do
    mMatches <- sequence $ fmap sequence $
      pairWith matches subpatterns subterms
    mconcat <$> lift mMatches

-- TODO: write context of var names?
matches (PatternPrimVal pVal) (PrimValue val)
  | pVal == Nothing
  = emptyMatch
  | pVal == Just val
  = emptyMatch
  | otherwise
  = noMatch

matches PatternAny _
  = emptyMatch
matches (PatternUnion pats) tm
  = do env <- ask
       lift $ getFirst $ foldMap
         (\pat -> First $ runReaderT (pat `matches` tm) env)
         pats
matches _ _
  = noMatch

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
minus x@PatternPrimVal{} _           = pure x
minus x@PatternTm{} PatternPrimVal{} = pure x

-- | Check a chart for uncovered and overlapping patterns.
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
  pretty (Operator name arity _) = case arity of
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

makeLenses ''SyntaxChart
makeLenses ''Sort
makePrisms ''Sort
makeLenses ''SortDef
makeLenses ''Arity
makeWrapped ''Arity
makeLenses ''Pattern
makePrisms ''Pattern
makeLenses ''Operator
makeLenses ''Term
makePrisms ''Term
deriveMatchable ''Term
makeLenses ''PatternCheckResult
