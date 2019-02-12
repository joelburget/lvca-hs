{-# LANGUAGE RecursiveDo     #-}
{-# LANGUAGE TemplateHaskell #-}
module Lvca.EarleyParseTerm (concreteParser) where

import           Control.Applicative  (many, (<|>))
import           Control.Lens         (ALens', ifor, ix, view, (<&>), (^?!))
import           Control.Lens.TH      (makeLenses)
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Data.Char            (isSpace)
import           Data.Foldable        (asum, toList)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Void            (Void)
import           Text.Earley
  (Grammar, Parser, Prod, listLike, parser, rule, satisfy, token, (<?>))

import           Lvca.Types           hiding (space)

data Parsers r = Parsers
  { _whitespaceParser :: !(Prod r Text Char String)
  , _higherPrecParser :: !(Prod r Text Char (Term Void))
  , _samePrecParser   :: !(Prod r Text Char (Term Void))
  }
makeLenses ''Parsers

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
      let opNames = fst . fst <$> precedenceLevel
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

          thisLevelProds = precedenceLevel <&> \((opName, subTmNames), directive) ->
            let parser' = case directive of
                  InfixDirective str fixity  -> parseInfix opName str fixity
                  MixfixDirective directive' -> do
                    prodMap <- parseMixfixDirective directive'

                    -- convert @Map Text (Term Void)@ to @[Term Void]@ by order
                    -- names appear in @subTmNames@ (which is the order they
                    -- occur in on the lhs of the concrete parser spec)
                    let prodList = prodMap <&> \m ->
                          subTmNames <&> \name ->
                            m ^?! ix name

                    pure $ Term opName <$> prodList
            in runReader parser' $ Parsers whitespace higherPrecP samePrecP

      --  parse any expression with this, or higher, precedence
      rule $ asum thisLevelProds
        <|> higherPrecP
        <?> prodTag

  -- enter the lowest precedence parser
  pure $ last allProds

parens :: Prod r e Char a -> Prod r e Char a
parens p = token '(' *> p <* token ')'

parseMixfixDirective
  :: MixfixDirective -> Reader (Parsers r) (Prod r Text Char (Map Text (Term Void)))
parseMixfixDirective = \case
  Literal text -> pure $ Map.empty <$ listLike text
  Sequence d1 d2 -> do
    d1' <- parseMixfixDirective d1
    d2' <- parseMixfixDirective d2
    pure $ Map.union <$> d1' <*> d2'
  Line             -> pure $ Map.empty <$ token '\n'
  Nest _ directive -> parseMixfixDirective directive
  Group  directive -> parseMixfixDirective directive
  d1 :<+ d2 -> do
    d1' <- parseMixfixDirective d1
    d2' <- parseMixfixDirective d2
    pure $ d1' <|> d2'
  SubTerm name -> fmap (Map.singleton name) <$> view higherPrecParser

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

_unused ::
  ( ALens' (Parsers r) (Prod r Text Char String)
  , ALens' (Parsers r) (Prod r Text Char (Term Void))
  )
_unused = (whitespaceParser, samePrecParser)
