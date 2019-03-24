module Lvca.ParseBidirectional where

import           Data.Map                      (Map)
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
    ctx <- parseCtx
    _ <- symbol "|-" <|> symbol ">>"
    body <- parseClause
    pure (ctx, body)
  _    <- symbol' "---" >> many (char '-') -- at least three dashes
  name <- optional $ parens $ Text.pack <$> some anySingle
  conc <- parseClause
  pure $ Rule hyps name conc

parseCtx :: BidirectionalParser (Map Text Term)
parseCtx = undefined

parseTerm :: BidirectionalParser Term
parseTerm = undefined

parseClause :: BidirectionalParser TypingClause
parseClause = do
  t1        <- parseTerm
  direction <- eitherP (Left <$> symbol "=>") (Right <$> symbol "<=")
  t2        <- parseTerm
  pure $ case direction of
    Left  _ -> InferenceRule $ t1 :=> t2
    Right _ -> CheckingRule  $ t1 :<= t2
