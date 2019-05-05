{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2018-2019 Joel Burget
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
  , _SortAp
  , NamedSort(..)
  , sortName
  , namedSort
  , SyntaxChart(..)
  , syntaxChartContents
  -- , termSort
  , SortDef(..)
  , sortOperators
  , sortVariables
  , sortSubst
  , Operator(..)
  , operatorName
  , operatorArity
  , Arity(..)
  , arityIndex
  , valence
  , valences
  , exampleArity
  , Valence(..)
  , valenceSubst
  , valenceSorts

  -- | Concrete syntax charts
  , MixfixDirective(..)
  , (>>:)
  , OperatorDirective(..)
  , Fixity(..)
  , Associativity(..)
  -- , ConcreteSyntax(..)
  -- , keywords
  -- , mkConcreteSyntax

  -- * Denotation charts
  -- | Denotational semantics definition.
  -- , DenotationChart(..)
  , pattern (:->)
  , pattern PatternAny
  , pattern PatternEmpty
  , MatchResult(..)

  -- * Terms / Values
  , Scope(..)
  , Term(..)
  -- ** Term prisms / traversals
  , _Term
  , _Var
  , _PrimValue
  , subterms
  , termName
  , identify

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
  , matches
  , runMatches
  , completePattern
  , minus
  -- , patternCheck
  -- , findMatch
  ) where

import           Codec.CBOR.Decoding       (decodeListLenOf, decodeWord)
import           Codec.CBOR.Encoding       (encodeListLen, encodeWord)
import           Codec.Serialise
import           Control.Lens              hiding
  (Empty, index, mapping, op, prism)
import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Aeson
  (FromJSON(..), ToJSON(..), Value(..), withArray)
import           Data.ByteString           (ByteString)
import           Data.Data                 (Data)
import           Data.Eq.Deriving
import           Data.Foldable             (foldlM)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
-- import           Data.Maybe                (fromMaybe)
import           Data.Monoid               (First(First, getFirst))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.String               (IsString(fromString))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc hiding (space)
import qualified Data.Vector               as Vector
import           GHC.Exts                  (IsList(..))
import           GHC.Generics              (Generic)
import           Prelude                   hiding (lookup)
import           Text.Show.Deriving

import           Lvca.Judgements
import           Lvca.Util                 as Util

-- syntax charts

data Sort = SortAp !SortName ![Sort]
  deriving (Eq, Show, Data)

instance IsString Sort where
  fromString s = SortAp (fromString s) []

sortSubst :: Map Text Sort -> Sort -> Sort
sortSubst varVals = \case
  SortAp name []       -> Map.findWithDefault (SortAp name []) name varVals
  SortAp name subSorts -> SortAp name $ sortSubst varVals <$> subSorts
  -- TODO: we need to cleanly separate variables from concrete sort names

-- | Named sorts appear in syntax charts so we can refer to them.
--
-- eg @t1: tm@
data NamedSort = NamedSort
  { _sortName  :: !Text
  , _namedSort :: !Sort
  } deriving (Eq, Show, Data)

-- | A syntax chart defines the abstract syntax of a language, specified by a
-- collection of operators and their arities. The abstract syntax provides a
-- systematic, unambiguous account of the hierarchical and binding structure of
-- the language.
--
-- @
-- typ := num                numbers
--        str                strings
--
-- exp := val(val)
--        plus(exp; exp)     addition
--        times(exp; exp)    multiplication
--        cat(exp; exp)      concatenation
--        len(exp)           length
--        let(exp; exp. exp) definition
--
-- val := num                numeral
--        str                literal
-- @
newtype SyntaxChart
  = SyntaxChart { _syntaxChartContents :: Map SortName SortDef }
  deriving (Eq, Show, Data)

-- | Sorts divide ASTs into syntactic categories. For example, programming
-- languages often have a syntactic distinction between expressions and
-- commands.
data SortDef = SortDef
  { _sortVariables :: ![Text]               -- ^ set of variables
  , _sortOperators :: ![Operator]           -- ^ set of operators
  } deriving (Eq, Show, Data)

-- | One of the fundamental constructions of a language. Operators are grouped
-- into sorts. Each operator has an /arity/, specifying its arguments.
data Operator = Operator
  { _operatorName   :: !OperatorName -- ^ operator name
  , _operatorArity  :: !Arity        -- ^ arity
  , _operatorSyntax :: ![OperatorDirective]
    -- ^ layouts (in order of preference / compactness)
  } deriving (Eq, Show, Data)

-- | An /arity/ specifies the sort of an operator and the number and valences
-- of its arguments.
--
-- eg @(exp. exp; nat)@.
--
-- To specify an arity, the resulting sort (the last @Exp@ above) is
-- unnecessary, since it's always clear from context.
data Arity
  = FixedArity    { _valences   :: ![Valence] }
  | VariableArity { _arityIndex :: !Text, _valence :: !Valence }
  deriving (Eq, Show, Data)

instance IsList Arity where
  type Item Arity    = Valence
  fromList           = FixedArity
  toList = \case
    FixedArity l    -> l
    VariableArity{} -> error "toList VariableArity"

-- | A /valence/ specifies the sort of an argument as well as the number and
-- sorts of the variables bound within it.
--
-- eg @exp. exp@.
data Valence = FixedValence
  { _valenceSorts  :: ![NamedSort] -- ^ the sorts of all bound variables
  , _valenceResult :: !NamedSort   -- ^ the resulting sort
  }
  | VariableValence
  { _valenceIndex  :: !Text
  , _valenceResult :: !NamedSort -- ^ the resulting sort
  } deriving (Eq, Show, Data)

-- | Apply a sort-substitution to a valence
valenceSubst :: Map Text Sort -> Valence -> Valence
valenceSubst m = \case
  FixedValence as b
    -> FixedValence (sortSubst' m <$> as) (sortSubst' m b)
  VariableValence index result
    -> VariableValence index (sortSubst' m result)

  where sortSubst' m (NamedSort name sort) = NamedSort name (sortSubst m sort)

-- | Traverse the sorts of both binders and body.
valenceSorts :: Traversal' Valence NamedSort
valenceSorts f = \case
  FixedValence sorts result
    -> FixedValence <$> traverse f sorts <*> f result
  VariableValence index result
    -> VariableValence index <$> f result

instance IsString NamedSort where
  fromString str = NamedSort (fromString str) (fromString str)

instance IsString Valence where
  fromString = FixedValence [] . fromString

-- | @exampleArity = 'Arity' ['Valence' [\"exp\", \"exp\"] \"exp\"]@
exampleArity :: Arity
exampleArity = FixedArity [ FixedValence ["exp", "exp"] "exp" ]

-- | Parsing / pretty-printing directive
data MixfixDirective
  = Literal !Text
  | Sequence !MixfixDirective !MixfixDirective
  | Line
  -- TODO: should this be called Indent?
  | Nest !Int !MixfixDirective
  | Group !MixfixDirective
  | (:<+) !MixfixDirective !MixfixDirective
  | VarName !Text
  | SubTerm !Text
  deriving (Eq, Show, Data)

instance Pretty MixfixDirective where
  pretty = \case
    Literal str  -> dquotes $ pretty str
    Sequence a b -> hsep [pretty a, pretty b]
    Line         -> hardline
    Nest i a     -> "nest" <+> pretty i <+> pretty a
    Group d      -> "group(" <> pretty d <> ")"
    a :<+ b      -> pretty a <+> "<" <+> pretty b
    VarName name -> pretty name
    SubTerm name -> pretty name

mixfixDirectiveKeywords :: MixfixDirective -> Set Text
mixfixDirectiveKeywords = \case
  Literal kw       -> Set.singleton kw
  Sequence a b     -> Set.union (mdk a) (mdk b)
  Line             -> Set.empty
  Nest _ directive -> mdk directive
  Group directive  -> mdk directive
  a :<+ b          -> Set.union (mdk a) (mdk b)
  VarName _        -> Set.empty
  SubTerm _        -> Set.empty
  where mdk = mixfixDirectiveKeywords

infixr 5 >>:
(>>:) :: MixfixDirective -> MixfixDirective -> MixfixDirective
a >>: b = Sequence a b

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
  deriving (Eq, Show, Data)

instance Pretty Fixity where
  pretty = \case
    Infixl -> "infixl"
    Infixr -> "infixr"
    Infix  -> "infix"

data Associativity
  = Assocl
  | Assocr
  deriving (Show, Eq, Data)

instance Pretty Associativity where
  pretty = \case
    Assocl -> "assocl"
    Assocr -> "assocr"

data OperatorDirective
  = InfixDirective  !Text !Fixity
  | MixfixDirective !MixfixDirective
  | AssocDirective  !Associativity
  deriving (Eq, Show, Data)

operatorDirectiveKeywords :: OperatorDirective -> Set Text
operatorDirectiveKeywords = \case
  InfixDirective kw _       -> Set.singleton kw
  MixfixDirective directive -> mixfixDirectiveKeywords directive
  AssocDirective{}          -> Set.empty

instance Pretty OperatorDirective where
  pretty = \case
    InfixDirective name fixity
      -> hsep [ pretty fixity, "x", dquotes (pretty name), "y" ]
    MixfixDirective dir
      -> pretty dir
    AssocDirective fixity
      -> hsep [ pretty fixity, "x", "y" ]

{-
ruleKeywords :: ConcreteSyntaxRule -> Set Text
ruleKeywords (ConcreteSyntaxRule _ _ directive)
  = operatorDirectiveKeywords directive

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

keywords :: ConcreteSyntax -> Set Text
keywords (ConcreteSyntax rules)
  = Set.unions $ fmap (Set.unions . fmap ruleKeywords) rules

instance Pretty ConcreteSyntax where
  pretty (ConcreteSyntax precLevels) = vsep $
    toList precLevels <&> \decls -> indent 2 $
      ("-" <+>) $ align $ vsep $ decls <&>
        \(ConcreteSyntaxRule op slots directive) -> hsep
          [ pretty op <> encloseSep "(" ")" "; " (map prettySlot slots)
          , "~"
          , pretty directive
          ]
    where prettySlot (binderNames, bodyName)
            = sep $ punctuate "." $ fmap pretty $ binderNames <> [bodyName]

mkConcreteSyntax :: [[ConcreteSyntaxRule]] -> ConcreteSyntax
mkConcreteSyntax = ConcreteSyntax . Seq.fromList
-}

data Scope a = Scope ![Text] !(Term a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | An evaluated or unevaluated term
data Term a
  = Term
    { _termName :: !Text      -- ^ name of this term
    , _subterms :: ![Scope a] -- ^ subterms
    }
  | Var !Text
  | PrimValue !a
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

arr :: [Value] -> Value
arr = Array . Vector.fromList

instance ToJSON a => ToJSON (Scope a) where
  toJSON (Scope names tm) = arr [toJSON names, toJSON tm]

instance ToJSON a => ToJSON (Term a) where
  toJSON = \case
    Term name subtms   -> arr [String "t", toJSON name, toJSON subtms]
    Var name           -> arr [String "v", toJSON name]
    PrimValue a        -> arr [String "p", toJSON a]

instance FromJSON a => FromJSON (Scope a) where
  parseJSON = withArray "Scope" $ \v -> case Vector.toList v of
    [names, tm] -> Scope <$> parseJSON names <*> parseJSON tm
    -- TODO: better message
    _           -> fail "unexpected JSON format for Scope"

instance FromJSON a => FromJSON (Term a) where
  parseJSON = withArray "Term" $ \v -> case Vector.toList v of
    [String "t", a, b] -> Term      <$> parseJSON a <*> parseJSON b
    [String "v", a   ] -> Var       <$> parseJSON a
    [String "p", a   ] -> PrimValue <$> parseJSON a
    -- TODO: better message
    _                  -> fail "unexpected JSON format for Term"

instance Serialise a => Serialise (Scope a) where
  encode (Scope names tm) = encodeListLen 2 <> encode names <> encode tm

  decode = do
    decodeListLenOf 2
    Scope <$> decode <*> decode

instance Serialise a => Serialise (Term a) where
  encode tm =
    let (tag, content) = case tm of
          Term      name  subtms -> (0, encode name <> encode subtms)
          Var       name         -> (1, encode name                 )
          PrimValue a            -> (2, encode a                    )
    in encodeListLen 2 <> encodeWord tag <> content

  decode = do
    decodeListLenOf 2
    tag <- decodeWord
    case tag of
      0 -> Term      <$> decode <*> decode
      1 -> Var       <$> decode
      2 -> PrimValue <$> decode
      _ -> fail "invalid Term encoding"

newtype Sha256 = Sha256 ByteString

-- | Content-identify a term via the sha-256 hash of its cbor serialization.
identify :: Serialise a => Term a -> Sha256
identify = Sha256 . SHA256.hashlazy . serialise

instance Pretty a => Pretty (Scope a) where
  pretty (Scope names tm)
    = hsep $ punctuate dot $ fmap pretty names <> [pretty tm]

instance Pretty a => Pretty (Term a) where
  pretty = \case
    Term name subtms ->
      pretty name <> parens (hsep $ punctuate semi $ fmap pretty subtms)
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
  | PatternVariableArity !(Pattern a)
  deriving (Eq, Show)

pattern PatternAny :: Pattern a
pattern PatternAny = PatternVar Nothing

pattern PatternEmpty :: Pattern a
pattern PatternEmpty = PatternUnion []

deriveShow1 ''Scope
deriveEq1   ''Scope
deriveShow1 ''Term
deriveEq1   ''Term
deriveShow1 ''Pattern
deriveEq1   ''Pattern

-- | The result of matching a pattern against a term. Example:
--
-- Pattern @lam(body)@ 'matches' term @lam(x. x)@, resulting in 'MatchResult'
-- @body -> x. x@.
newtype MatchResult a = MatchResult { _assignments :: Map Text (Scope a) }
  deriving (Eq, Show, Semigroup, Monoid)

-- | Is this pattern redundant?
data IsRedudant = IsRedudant | IsntRedundant
  deriving Eq

-- | A sequence of patterns can be checked for uncovered patterns and overlap.
data PatternCheckResult a = PatternCheckResult
  { _uncovered   :: !(Pattern a)               -- ^ uncovered patterns
  , _overlapping :: ![(Pattern a, IsRedudant)] -- ^ overlapping part, redundant?
  }

type instance Index   (Term a) = Int
type instance IxValue (Term a) = Scope a
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

-- | The inverse of 'matches'. Use caution when using this function:
--
-- * It's unhygienic and subject to variable capture.
-- * There are very few use cases.
--
-- Most of the time you want to hygienically open a term.
applySubst :: Show a => MatchResult a -> Term a -> Term a
applySubst subst@(MatchResult assignments) tm = case tm of
  Term name subtms    -> Term name (applySubst' subst <$> subtms)
  Var name            -> case assignments ^? ix name of
    Just (Scope _ tm') -> tm'
    _                  -> tm
  _                   -> tm

applySubst' :: Show a => MatchResult a -> Scope a -> Scope a
applySubst' subst (Scope names subtm) = Scope names (applySubst subst subtm)

-- | Match any instance of this operator
toPattern :: Operator -> Pattern a
toPattern (Operator name (FixedArity valences) _layouts)
  = PatternTm name $ valences <&> \case
      VariableValence _n _ -> PatternVariableArity PatternAny
      _valence             -> PatternAny

data MatchesEnv a = MatchesEnv
  { _envChart :: !SyntaxChart
  -- ^ The language we're working in
  , _envSort  :: !SortName
  -- ^ The specific sort we're matching
  }

makeLenses ''MatchesEnv

type Matching a = ReaderT (MatchesEnv a) Maybe

noMatch, emptyMatch :: Matching a (MatchResult a)
noMatch    = lift Nothing
emptyMatch = pure mempty

matches :: (Show a, Eq a) => Pattern a -> Term a -> Matching a (MatchResult a)
matches (PatternVar (Just name)) tm
  = pure $ MatchResult $ Map.singleton name (Scope [] tm)
matches (PatternVar Nothing)     _
  = emptyMatch

matches (PatternTm name1 subpatterns) (Term name2 subterms)
  | name1 /= name2
  = noMatch
  | otherwise = do
    mMatches <- sequence $ fmap sequence $ pairWith
      (\pat (Scope names tm)
        -> MatchResult . fmap (enscope names) . _assignments <$> matches pat tm)
      subpatterns
      subterms
    mconcat <$> lift mMatches

-- TODO: write context of var names?
matches (PatternPrimVal pVal) (PrimValue val)
  | pVal == Nothing
  = emptyMatch
  | pVal == Just val
  = emptyMatch
  | otherwise
  = noMatch

matches (PatternUnion pats) tm
  = do env <- ask
       lift $ getFirst $ foldMap
         (\pat -> First $ runReaderT (pat `matches` tm) env)
         pats
matches _ _
  = noMatch

enscope :: [Text] -> Scope a -> Scope a
enscope binders (Scope binders' body) = Scope (binders' <> binders) body

runMatches :: SyntaxChart -> SortName -> Matching b a -> Maybe a
runMatches chart sort = flip runReaderT (MatchesEnv chart sort)

getSort :: Matching a SortDef
getSort = do
  MatchesEnv (SyntaxChart syntax) sortName <- ask
  lift $ syntax ^? ix sortName

completePattern :: Matching a (Pattern a)
completePattern = do
  SortDef _vars operators <- getSort
  pure $ PatternUnion $ toPattern <$> operators

minus :: Eq a => Pattern a -> Pattern a -> Matching a (Pattern a)
minus _ (PatternVar _) = pure PatternEmpty
minus (PatternVar _) x = do
  pat <- completePattern
  minus pat x

minus x@(PatternTm hd subpats) (PatternTm hd' subpats')
  | hd == hd' = do
    subpats'' <- sequence $ zipWith minus subpats subpats'
    pure $ if all (== PatternEmpty) subpats''
       then PatternEmpty
       else PatternTm hd subpats''
  | otherwise = pure x

minus (PatternUnion pats) x = do
  pats' <- traverse (`minus` x) pats
  pure $ PatternUnion $ filter (/= PatternEmpty) pats'

-- remove each pattern from pat one at a time
minus pat (PatternUnion pats) = foldlM minus pat pats

minus x@(PatternPrimVal a) (PatternPrimVal b)
  = pure $ if a == b then PatternEmpty else x
minus x@PatternPrimVal{} _           = pure x
minus x@PatternTm{} PatternPrimVal{} = pure x

instance Pretty SyntaxChart where
  -- TODO: show start sort?
  pretty (SyntaxChart sorts) =
    let f (title, SortDef vars operators) = vsep
          [ let vars' = case vars of
                  [] -> ""
                  _  -> " " <> hsep (fmap pretty vars)
            in pretty title <> vars' <> " :="
          , indent 2 $ vsep $ fmap pretty operators
          ]
    in vsep $ f <$> Map.toList sorts

instance Pretty Operator where
  pretty (Operator name arity layouts) =
    let line1 = ("|" <+>) $ pretty name <> pretty arity
        layoutLines = fmap (\layout -> "~" <+> pretty layout) layouts
    in vsep $ line1 : layoutLines

instance Pretty Arity where
  pretty = \case
    FixedArity valences -> case valences of
      [] -> mempty
      _  -> parens $ hsep $ punctuate semi $ fmap pretty valences
    VariableArity index valence ->
      brackets (pretty index) <> pretty valence

instance Pretty Sort where
  pretty (SortAp name args) = hsep $ pretty name : fmap pretty args

instance Pretty NamedSort where
  pretty (NamedSort name sort@(SortAp name' args))
    | name == name' && args == []
    = pretty name
    | otherwise
    = pretty name <> ": " <> pretty sort

instance Pretty Valence where
  pretty = \case
    FixedValence boundVars result -> mconcat $
      punctuate dot (fmap pretty boundVars <> [pretty result])
    VariableValence index result ->
      pretty result <> braces (pretty index)

makeLenses ''SyntaxChart
makeLenses ''Sort
makePrisms ''Sort
makeLenses ''NamedSort
makeLenses ''SortDef
makeLenses ''Arity
makeLenses ''Pattern
makePrisms ''Pattern
makeLenses ''Operator
makeLenses ''Term
makePrisms ''Term
makeLenses ''PatternCheckResult
