module Lvca.Syntax.Types

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

operatorDirectiveKeywords :: OperatorDirective -> Set Text
operatorDirectiveKeywords = \case
  InfixDirective kw _       -> Set.singleton kw
  MixfixDirective directive -> mixfixDirectiveKeywords directive
  AssocDirective{}          -> Set.empty

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
-}
