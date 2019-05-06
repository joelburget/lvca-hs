module Lvca.ParseConcreteSyntaxDescription where

import           Data.Char                     (isAscii, isLower, isUpper)
import           Data.Foldable                 (asum)
import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, pack)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char          (digitChar)
import qualified Text.Megaparsec.Char.Lexer    as L

import           Lvca.Judgements (SortName)
import qualified Lvca.ParseUtil  as PU
import           Lvca.ParseUtil  (endBy', sc, scn, symbol, symbol')
import           Lvca.Types      (Fixity(..) , Associativity(..))
import           Text.Megaparsec.Char          (char)

type Name = Text
type Nat  = Integer

data ConcreteSyntax = ConcreteSyntax
  { syntaxTerminals    :: ![TerminalRule]    -- (Map Text OperatorDirective)
  , syntaxNonterminals :: ![NonterminalRule] -- (Map Text Regex)
  } deriving Show

type ConcreteSyntaxDescriptionParser a = Parsec
  Void -- error type
  Text -- stream type
  a

upperChar :: ConcreteSyntaxDescriptionParser Char
upperChar = satisfy (\c -> isUpper c && isAscii c) <?> "[A-Z]"

lowerChar :: ConcreteSyntaxDescriptionParser Char
lowerChar = satisfy (\c -> isLower c && isAscii c) <?> "[a-z]"

terminalIdent :: ConcreteSyntaxDescriptionParser Text
terminalIdent = pack
  <$> ((:)
       <$> upperChar
       <*> many (upperChar <|> digitChar <|> char '_' <|> char '-'
         <?> "[-_A-Z0-9]")
       <*  sc)
  <?> "TERMINAL_IDENT"

nonterminalIdent :: ConcreteSyntaxDescriptionParser Text
nonterminalIdent = pack
  <$> ((:)
       <$> lowerChar
       <*> many (lowerChar <|> digitChar <|> char '_' <|> char '-'
         <?> "[-_a-z0-9]")
       <*  sc)
  <?> "NONTERMINAL_IDENT"

stringLiteral :: ConcreteSyntaxDescriptionParser Text
stringLiteral = PU.stringLiteral <* sc -- TODO: audit
  <?> "STRING_LITERAL"

metachars :: String
metachars = "\\|*+.[]()"

parseRegularChar :: ConcreteSyntaxDescriptionParser Char
-- TODO: more comprehensive list of whitespace chars
parseRegularChar = noneOf (" \t\n" <> metachars) <?> "REGULAR_CHAR"

parseMetachar :: ConcreteSyntaxDescriptionParser Char
parseMetachar = oneOf metachars <?> "METACHAR"

-- | Parse a concrete syntax description, eg:
--
-- > TRUE  := true
-- > FALSE := false
-- >
-- > tm :=
-- >   | val(val)              ~ val
-- >   | or(v1: val; v2: val)  ~ v1 "||" v2
-- >   | and(v1: val; v2: val) ~ v1 "&&" v2
-- >   | not(val)              ~ "~" val
-- > val :=
-- >   | true()                ~ TRUE
-- >   | false()               ~ FALSE
-- > ty := bool()              ~ "bool"
syntaxDescription
  :: ConcreteSyntaxDescriptionParser ConcreteSyntax
syntaxDescription = ConcreteSyntax
  <$> many (terminalRule <* scn)
  <*> some nonterminalRule
  <?> "syntax-description"

-- terminal-rule
data TerminalRule = TerminalRule !Text !Regex
  deriving Show

-- | Parse a terminal description, eg:
--
-- > TRUE := true
terminalRule :: ConcreteSyntaxDescriptionParser TerminalRule
terminalRule = TerminalRule <$> terminalIdent <* symbol' ":=" <*> regex
  <?> "terminal-rule"

-- nonterminal-rule
data NonterminalRule = NonterminalRule
  !SortName
  ![Text]
  ![NonterminalCtor]
  deriving Show

-- | Parse a nonterminal description, eg:
--
-- > val :=
-- > | true()  ~ TRUE
-- > | false() ~ FALSE
nonterminalRule :: ConcreteSyntaxDescriptionParser NonterminalRule
nonterminalRule = label "nonterminal-rule" $ do
  name <- nonterminalIdent
  vars <- many nonterminalIdent
  _    <- symbol ":="
  rhs  <- nonterminalCtors
  scn

  pure $ NonterminalRule name vars rhs

-- nonterminal-ctors
nonterminalCtors :: ConcreteSyntaxDescriptionParser [NonterminalCtor]
nonterminalCtors = asum
  [ do indentBlock $ symbol' "|" *> nonterminalCtor
  -- , (nonterminalCtor `sepBy` symbol' "|") <* eol
  ] <?> "nonterminal-ctors"

indentBlock
  :: Show a
  => ConcreteSyntaxDescriptionParser a
  -> ConcreteSyntaxDescriptionParser [a]
indentBlock parseA = do
  scn
  level <- L.indentLevel
  indentedItems level parseA

indentedItems
  :: Show b
  => Pos                                 -- ^ Level of the first indented item ('lookAhead'ed)
  -> ConcreteSyntaxDescriptionParser b   -- ^ How to parse indented tokens
  -> ConcreteSyntaxDescriptionParser [b]
indentedItems lvl p = go
  where
    go = do
      sc
      pos  <- L.indentLevel
      done <- isJust <$> optional eof
      if done
        then return []
        else if | pos <  lvl -> return []
                | pos == lvl -> (:) <$> p <*> go
                | otherwise  -> mempty -- incorrectIndent EQ lvl pos

data NonterminalCtor = NonterminalCtor
  !AbstractPat
  ![NonterminalMatch]
  deriving Show

nonterminalCtor :: ConcreteSyntaxDescriptionParser NonterminalCtor
nonterminalCtor = label "nonterminal-ctor" $ do
  pat <- abstractPat
  scn
  matches <- many $ symbol' "~" *> nonterminalMatch <* scn
  pure $ NonterminalCtor pat matches

data AbstractPat = AbstractPat
  !Name
  !(Maybe FNat)
  ![AbstractValence]
  deriving Show

abstractPat :: ConcreteSyntaxDescriptionParser AbstractPat
abstractPat = label "abstract-pat" $ do
  name <- nonterminalIdent
  n    <- optional $ symbol' "[" *> fnat <* symbol' "]"
  args <- symbol' "(" *> abstractValence `sepBy` symbol' ";" <* symbol' ")"
  pure $ AbstractPat name n args

data AbstractValence
  = AbstractValence ![NamedSort] !NamedSort
  deriving Show

abstractValence :: ConcreteSyntaxDescriptionParser AbstractValence
abstractValence = AbstractValence
  <$> namedSort `endBy'` symbol' "."
  <*> namedSort
  <?> "abstract-valence"

data NamedSort = NamedSort !(Maybe Name) !Sort !(Maybe FNat)
  deriving Show

namedSort :: ConcreteSyntaxDescriptionParser NamedSort
namedSort = label "named-sort" $ do
  name  <- optional $ try $ nonterminalIdent <* symbol' ":"
  sort' <- sort
  n     <- optional $ symbol' "[" *> fnat <* symbol' "]"
  pure $ NamedSort name sort' n

data Sort
  = SortName !Name
  | SortAp !Sort !Sort
  deriving Show

sort :: ConcreteSyntaxDescriptionParser Sort
sort = foldr1 SortAp <$> some sortAtom <?> "sort"

sortAtom :: ConcreteSyntaxDescriptionParser Sort
sortAtom = label "sortAtom" $ asum
  [ symbol' "(" *> sort <* symbol' ")"
  , SortName <$> nonterminalIdent
  ]

data FNat
  = Freenat !Name
  | ConcreteNat !Nat
  deriving Show

fnat :: ConcreteSyntaxDescriptionParser FNat
fnat = asum
  [ Freenat     <$> nonterminalIdent
  , ConcreteNat <$> L.decimal
  ]

data NonterminalMatch
  = AssociativeMatch !Associativity
  | InfixMatch !Name !Fixity
  | MixfixMatch ![NonterminalToken]
  deriving Show

nonterminalMatch :: ConcreteSyntaxDescriptionParser NonterminalMatch
nonterminalMatch = asum
  [ AssociativeMatch <$> associativity
  , try $ InfixMatch <$> nonterminalIdent <*> fixity
  , MixfixMatch      <$> some nonterminalToken
  ] <?> "nonterminal-match"

associativity :: ConcreteSyntaxDescriptionParser Associativity
associativity = asum
  [ Assocl <$ symbol' "assocl"
  , Assocr <$ symbol' "assocr"
  ] <?> "associativity"

fixity :: ConcreteSyntaxDescriptionParser Fixity
fixity = asum
  [ Infixl <$ symbol' "infixl"
  , Infixr <$ symbol' "infixr"
  , Infix  <$ symbol' "infix"
  ] <?> "fixity"

data NonterminalToken
  = TerminalName !Text
  | NonterminalName !Text
  | StringLiteral !Text
  | NtParenthesized !NonterminalScope
  | NtOption !NonterminalToken
  | NtStarred !NonterminalToken
  | NtPlussed !NonterminalToken
  | NtIndexed !NonterminalToken !FNat
  | NtCount !NonterminalToken !(Maybe Name) !FNat
  | Ellipsis
  deriving Show

nonterminalToken :: ConcreteSyntaxDescriptionParser NonterminalToken
nonterminalToken = label "nonterminal-token" $ do
  ntAtom   <- nonterminalAtom
  modifier <- optional nonterminalModifier
  pure $ case modifier of
    Nothing              -> ntAtom
    Just AtomOption      -> NtOption ntAtom
    Just AtomStarred     -> NtStarred ntAtom
    Just AtomPlussed     -> NtPlussed ntAtom
    Just (AtomIndex n)   -> NtIndexed ntAtom n
    Just (AtomCount i n) -> NtCount ntAtom i n

nonterminalAtom :: ConcreteSyntaxDescriptionParser NonterminalToken
nonterminalAtom = asum
  [ TerminalName    <$> terminalIdent
  , NonterminalName <$> nonterminalIdent
  , StringLiteral   <$> stringLiteral
  , NtParenthesized <$> (symbol' "(" *> nonterminalScope <* symbol' ")" <* sc)
  , Ellipsis <$ symbol' "..."
  ] <?> "nonterminal-atom"

data NonterminalModifier
  = AtomOption
  | AtomStarred
  | AtomPlussed
  | AtomIndex !FNat
  | AtomCount !(Maybe Name) !FNat

nonterminalModifier :: ConcreteSyntaxDescriptionParser NonterminalModifier
nonterminalModifier = asum
  [ AtomOption  <$ symbol' "?"
  , AtomStarred <$ symbol' "*"
  , AtomPlussed <$ symbol' "+"
  , AtomIndex <$> (symbol' "[" *> fnat <* symbol' "]")
  , do
    _ <- symbol' "{"
    i <- optional $ nonterminalIdent <* symbol' ":"
    n <- fnat
    _ <- symbol' "}"
    pure $ AtomCount i n
  ] <?> "nonterminal-modifier"

data NonterminalScope = NonterminalScope
  ![(NonterminalToken, Binder)]
  ![NonterminalToken]
  deriving Show

nonterminalScope :: ConcreteSyntaxDescriptionParser NonterminalScope
nonterminalScope = do
  binders <- ((,) <$> nonterminalToken <*> binder) `endBy'` symbol' "."
  toks    <- many nonterminalToken
  pure $ NonterminalScope binders toks

data Binder
  = SingleVarBinder !Name ![NonterminalToken]
  | ManyVarBinder !Nat
  deriving Show

binder :: ConcreteSyntaxDescriptionParser Binder
binder = do
  _ <- symbol' "VAR"
  asum
    [ do _    <- symbol' ":"
         name <- nonterminalIdent
         toks <- many nonterminalToken
         pure $ SingleVarBinder name toks
    -- , TODO
    ]

data Regex
  = ReUnion  !RegexBranch !Regex
  | ReBranch !RegexBranch
  deriving Show

newtype RegexBranch = Pieces [Piece]
  deriving Show

regex :: ConcreteSyntaxDescriptionParser Regex
regex = label "regex" $ do
  b1 <- regexBranch
  option (ReBranch b1) $ ReUnion b1 <$> (symbol' "|" *> regex)

regexBranch :: ConcreteSyntaxDescriptionParser RegexBranch
regexBranch = Pieces <$> many piece
  <?> "regex-branch"

data Piece
  = Atom    !Atom
  | Option  !Atom
  | Starred !Atom
  | Plussed !Atom
  deriving Show

piece :: ConcreteSyntaxDescriptionParser Piece
piece = label "regex-piece" $ do
  a        <- atom
  modifier <- optional (symbol' "?" <|> symbol' "*" <|> symbol' "+")
  case modifier of
    Nothing  -> pure $ Atom    a
    Just "?" -> pure $ Option  a
    Just "*" -> pure $ Starred a
    Just "+" -> pure $ Plussed a
    Just _   -> error "vacuous match"

data Atom
  = PositiveSet   ![SetItem]
  | NegativeSet   ![SetItem]
  | Parenthesized !Regex
  | Escaped       !Char
  | Char          !Char
  | Any
  deriving Show

atom :: ConcreteSyntaxDescriptionParser Atom
atom = asum
  [ NegativeSet   <$> (symbol' "[^" *> some setItem <* symbol' "]")
  , PositiveSet   <$> (symbol' "["  *> some setItem <* symbol' "]")
  , Parenthesized <$> (symbol' "("  *> regex        <* symbol' ")")
  , Any           <$  symbol' "."
  , Escaped       <$> (char '\\' *> parseMetachar)
  , Char          <$> parseRegularChar
  ] <?> "regex-atom"

data SetItem
  = SiRange !Char !Char
  | SiChar  !Char
  deriving Show

parsePossiblyEscapedChar :: ConcreteSyntaxDescriptionParser Char
parsePossiblyEscapedChar = asum
  [ char '\\' *> parseMetachar
  , parseRegularChar
  ] <?> "possibly-escaped-char"

setItem :: ConcreteSyntaxDescriptionParser SetItem
setItem = label "regex-set-item" $ do
  c <- parsePossiblyEscapedChar
  option (SiChar c) $ SiRange c <$> (symbol' "-" *> parsePossiblyEscapedChar)
