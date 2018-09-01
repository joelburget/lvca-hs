{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TupleSections          #-}
module Linguist.ParseUtil where

import Control.Monad (void)
import Data.Text (Text)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- TODO: deduplicate sc, lexeme, symbol, parens
lineComment, blockComment
  :: MonadParsec e Text m => m ()
lineComment  = L.skipLineComment "//"
blockComment = L.skipBlockComment "/*" "*/"

scn :: MonadParsec e Text m => m ()
scn = L.space space1 lineComment blockComment

sc :: MonadParsec e Text m => m ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment blockComment
  where
    f x = x == ' ' || x == '\t'

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme sc

symbol :: MonadParsec e Text m => Text -> m Text
symbol = L.symbol sc

parens :: MonadParsec e Text m => m a -> m a
parens = between (symbol "(") (symbol ")")
