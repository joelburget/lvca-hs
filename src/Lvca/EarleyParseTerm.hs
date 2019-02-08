{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TemplateHaskell            #-}
module Lvca.EarleyParseTerm (concreteParser) where

import           Control.Applicative  ((<|>), many)
import           Control.Lens         (review, ifor, (<&>), view, ALens')
import           Control.Lens.TH      (makeLenses)
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Data.Char            (isSpace)
import           Data.Foldable        (asum, toList)
import           Data.Text            (Text)
import           Data.Void            (Void)
import qualified Data.Text            as Text
import           Text.Earley          (Prod, Grammar, rule, parser, (<?>),
                                       listLike, token, Parser, satisfy)

import           Lvca.Types           hiding (space)

data Parsers r = Parsers
  { _whitespaceParser :: !(Prod r Text Char String)
  , _higherPrecParser :: !(Prod r Text Char (Term Void))
  , _samePrecParser   :: !(Prod r Text Char (Term Void))
  }
makeLenses ''Parsers

_unused ::
  ( ALens' (Parsers r) (Prod r Text Char String)
  , ALens' (Parsers r) (Prod r Text Char (Term Void))
  )
_unused = (whitespaceParser, samePrecParser)

-- | Parse 'Text' to a 'Term' for some 'ConcreteSyntax'
concreteParser :: ConcreteSyntax -> Parser Text Text (Term Void)
concreteParser stx = parser (concreteParserGrammar stx)

concreteParserGrammar
  :: forall r.
     ConcreteSyntax
  -> Grammar r (Prod r Text Char (Term Void))
concreteParserGrammar (ConcreteSyntax directives) = mdo
  whitespace <- rule $ many $ satisfy isSpace

  allProds <- mfix $ \prods ->
    ifor (toList directives) $ \precedence precedenceLevel -> do
      let opNames = fst <$> precedenceLevel
          prodTag = Text.intercalate " | " opNames

          -- parsers for every operator sharing this precedence (note the
          -- knot-tying)
          samePrecP = prods !! precedence

          -- If this is the highest precedence level then we allow
          -- subexpressions of any precedence with parens, otherwise we allow
          -- (unparenthesized) subexpressions of higher precedence (which of
          -- course include parenthesized expressions)
          higherPrecP = if precedence == 0
            then parens (last prods)
            else prods !! pred precedence

          thisLevelProds = precedenceLevel <&> \(opName, directive) ->
            let parser' = case directive of
                  InfixDirective str fixity  -> parseInfix opName str fixity
                  MixfixDirective directive' -> parseMixfixDirective $
                    Some directive' id
            in runReader parser' $ Parsers whitespace higherPrecP samePrecP

      --  parse any expression with this, or higher, precedence
      rule $ asum thisLevelProds
        <|> higherPrecP
        <?> prodTag

  -- enter the lowest precedence parser
  pure $ last allProds

parens :: Prod r e Char a -> Prod r e Char a
parens p = token '(' *> p <* token ')'

data SomeDirective b where
  Some :: MixfixDirective a -> (a -> b) -> SomeDirective b

parseMixfixDirective
  :: SomeDirective a -> Reader (Parsers r) (Prod r Text Char a)
parseMixfixDirective = \case
  Some (Literal text) f -> pure $ f () <$ listLike text
  Some (Sequence d1 d2) f -> do
    d1' <- parseMixfixDirective $ Some d1 id
    d2' <- parseMixfixDirective $ Some d2 id
    pure $ fmap f $ (,) <$> d1' <*> d2'
  Some Line               f -> pure $ f () <$ token '\n' -- TODO
  Some (Nest _ directive) f -> parseMixfixDirective $ Some directive f
  Some (Group  directive) f -> parseMixfixDirective $ Some directive f
  Some (d1 :<+ d2) f -> do
    d1' <- parseMixfixDirective $ Some d1 f
    d2' <- parseMixfixDirective $ Some d2 f
    pure $ d1' <|> d2'
  Some (PrismD p' d) f -> parseMixfixDirective $ Some d $ f . review p'
  Some (SubTerm _sortName) f -> fmap f <$> view higherPrecParser

-- | Parse an infix operator
parseInfix
  :: Text -- ^ Operator name (in abstract syntax), eg @"Add"@
  -> Text -- ^ Operator representation (in concrete syntax), eg @"+"@
  -> Fixity
  -> Reader (Parsers r) (Prod r Text Char (Term Void))
parseInfix opName opRepr fixity
  = reader $ \(Parsers whitespace higherPrec samePrec) ->
    -- Allow expressions of the same precedence on the side we're associative
    -- on
    let (subparser1, subparser2) = case fixity of
          Infixl -> (samePrec  , higherPrec)
          Infix  -> (higherPrec, higherPrec)
          Infixr -> (higherPrec, samePrec  )
    in BinaryTerm opName
         <$> subparser1
         <*  whitespace
         <*  listLike opRepr
         <*  whitespace
         <*> subparser2
