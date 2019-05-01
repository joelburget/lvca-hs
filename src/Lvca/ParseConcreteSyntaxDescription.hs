module Lvca.ParseConcreteSyntaxDescription where

import           Data.Char                     (isAscii, isLower, isUpper)
import           Data.Foldable                 (asum)
import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, pack)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char          (digitChar)
import qualified Text.Megaparsec.Char.Lexer    as L

-- import           Lvca.ParseDenotationChart     (parsePattern)
import           Lvca.Judgements (SortName)
import qualified Lvca.ParseUtil  as PU
import           Lvca.ParseUtil  (endBy', sc, scn, symbol, symbol', parseName)
import           Lvca.Types      (Fixity(..) , Associativity(..))
-- import           Lvca.Types                    hiding (Scope)
import           Text.Megaparsec.Char          (char, eol)

import Text.Megaparsec.Debug

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
         <?> "[A-Z0-9_-]")
       <*  sc)
  <?> "TERMINAL_IDENT"

nonterminalIdent :: ConcreteSyntaxDescriptionParser Text
nonterminalIdent = pack
  <$> ((:)
       <$> lowerChar
       <*> many (lowerChar <|> digitChar <|> char '_' <|> char '-'
         <?> "[a-z0-9_-]")
       <*  sc)
  <?> "NONTERMINAL_IDENT"

stringLiteral :: ConcreteSyntaxDescriptionParser Text
stringLiteral = PU.stringLiteral <* sc -- TODO: audit
  <?> "STRING_LITERAL"

metachars :: String
metachars = "\\|*+.[()"

parseRegularChar :: ConcreteSyntaxDescriptionParser Char
parseRegularChar = noneOf metachars <?> "REGULAR_CHAR"

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
  <$> many terminalRule
  <*> some nonterminalRule
  <?> "syntax-description"

-- terminal-rule
data TerminalRule = TerminalRule !Text !Regex
  deriving Show

-- | Parse a terminal description, eg:
--
-- > TRUE := true
terminalRule :: ConcreteSyntaxDescriptionParser TerminalRule
terminalRule = TerminalRule <$> terminalIdent <* symbol' ":=" <*> regex <* scn
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
  !(Maybe Nat)
  ![AbstractArg]
  deriving Show

abstractPat :: ConcreteSyntaxDescriptionParser AbstractPat
abstractPat = label "abstract-pat" $ do
  name <- parseName
  n    <- optional $ symbol' "[" *> L.decimal <* symbol' "]" -- TODO: fnat?
  args <- symbol' "(" *> abstractArg `sepBy` symbol' ";" <* symbol' ")"
  pure $ AbstractPat name n args

data AbstractArg = AbstractArg
  !(Maybe Name)
  !Name
  ![Name]
  !(Maybe FNat)
  deriving Show

abstractArg :: ConcreteSyntaxDescriptionParser AbstractArg
abstractArg = do
  name1 <- parseName
  asum
    [ do _        <- symbol' ":"
         termName <- parseName
         option (AbstractArg (Just name1) termName [] Nothing) $ do
           args <- many parseName
           n    <- optional $ symbol' "[" *> fnat <* symbol' "]"
           pure $ AbstractArg (Just name1) termName args n
    , pure $ AbstractArg Nothing name1 [] Nothing
    ]

data FNat
  = Freenat !Name
  | ConcreteNat !Nat
  deriving Show

fnat :: ConcreteSyntaxDescriptionParser FNat
fnat = asum
  [ Freenat     <$> parseName
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
  , try $ InfixMatch       <$> parseName <*> fixity
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
  | NtCount !NonterminalToken !(Maybe Name) !(Either Text Nat)
  | Ellipsis
  deriving Show

nonterminalToken :: ConcreteSyntaxDescriptionParser NonterminalToken
nonterminalToken = asum
  [ TerminalName    <$> terminalIdent
  , NonterminalName <$> nonterminalIdent
  , StringLiteral   <$> stringLiteral
  , NtParenthesized <$> (symbol' "(" *> nonterminalScope <* symbol' ")" <* sc)
  -- XXX
  -- , NtOption        <$> nonterminalToken <* symbol' "?"
  -- , NtStarred       <$> nonterminalToken <* symbol' "*"
  -- , NtPlussed       <$> nonterminalToken <* symbol' "+"
  -- NtCount TODO
  , Ellipsis <$ symbol' "..."
  ] <?> "nonterminal-token"

data NonterminalScope = NonterminalScope
  ![(NonterminalToken, Binder)]
  ![NonterminalToken]
  deriving Show

nonterminalScope :: ConcreteSyntaxDescriptionParser NonterminalScope
nonterminalScope = do
  binders <- ((,) <$> nonterminalToken <*> binder)
    `endBy'` symbol' "."
  toks <- many nonterminalToken
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
         name <- parseName
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
    Just _   -> fail "expected \"?\", \"*\", or \"+\""

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
  -- TODO Escaped, Char
  , Any <$ symbol' "."
  ] <?> "regex-atom"

data SetItem
  = SiRange !Char !Char
  | SiChar  !Char
  deriving Show

setItem :: ConcreteSyntaxDescriptionParser SetItem
setItem = label "regex-set-item" $ do
  c <- parseRegularChar
  -- XXX parseRegularChar
  option (SiChar c) $ SiRange c <$> (symbol' "-" *> parseRegularChar)
