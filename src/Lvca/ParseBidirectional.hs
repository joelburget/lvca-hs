module Lvca.ParseBidirectional where

import           Control.Lens                  ((<&>))
import           Data.List                     (elemIndex)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Lvca.Bidirectional
import           Lvca.ParseUtil

type BidirectionalParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

parseBidirectional :: BidirectionalParser [Rule]
parseBidirectional = many parseRule

parseRule :: BidirectionalParser Rule
parseRule = do
  hyps <- many $ do
    ctx  <- parseCtx
    _    <- symbol "|-" <|> symbol ">>"
    body <- parseClause
    pure (ctx, body)
  _    <- symbol' "---" >> many (char '-') >> sc -- at least three dashes
  name <- optional $ parens $ fmap Text.pack $ some $ satisfy $ \c
    -> c /= '\n'
    && c /= '\r'
    && c /= ')'
  conc <- do
    _ <- symbol "ctx"
    _ <- symbol "|-" <|> symbol ">>"
    parseClause
  pure $ Rule hyps name conc

parseCtx :: BidirectionalParser (Map Text Term)
parseCtx = do
  _ <- symbol' "ctx"
  ctxInsertions <- many $ do
    _    <- symbol' ","
    name <- parseName
    _    <- symbol' ":"
    ty   <- parseTerm
    pure (name, ty)
  pure $ Map.fromList ctxInsertions

parseTerm :: BidirectionalParser Term
parseTerm = do
  name <- parseName
  option (Free name) $ parens $ fmap (Term name) $ parseScope `sepBy` symbol ";"

parseScope :: BidirectionalParser Scope
parseScope = do
  names <- parseName `endBy'` symbol' "."
  body  <- parseTerm
  pure $ close names body

close :: [Text] -> Term -> Scope
close names body = Scope names $ close' 0 body where
  close' offset tm = case tm of
    Term tag scopes
      -> Term tag $ scopes <&> \(Scope binders body')
        -> Scope binders $ close' (offset + length binders) body'
    Bound{}
      -> tm
    Free v
      | Just i <- elemIndex v names
      -> Bound $ i + offset
      | otherwise
      -> tm

parseClause :: BidirectionalParser TypingClause
parseClause = do
  t1        <- parseTerm
  direction <- symbol' "=>" `eitherP` symbol' "<="
  t2        <- parseTerm
  pure $ case direction of
    Left  _ -> InferenceRule $ t1 :=> t2
    Right _ -> CheckingRule  $ t1 :<= t2
