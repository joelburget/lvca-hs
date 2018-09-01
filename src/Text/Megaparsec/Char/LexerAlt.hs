{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Text.Megaparsec.Char.LexerAlt where

import Control.Applicative
import Data.Maybe (fromMaybe, isJust)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer hiding (indentBlock)


-- Same as megaparsec's @indentBlock@, but don't require a newline before the
-- indented block
indentBlock :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
indentBlock sc r = do
  sc
  ref <- indentLevel
  a   <- r
  case a of
    IndentNone x -> sc *> return x
    IndentMany indent f p -> do
      mlvl <- (optional . try) (indentGuard sc GT ref)
      done <- isJust <$> optional eof
      case (mlvl, done) of
        (Just lvl, False) ->
          indentedItems ref (fromMaybe lvl indent) sc p >>= f
        _ -> sc *> f []
    IndentSome indent f p -> do
      pos <- indentGuard sc GT ref
      let lvl = fromMaybe pos indent
      x <- if | pos <= ref -> incorrectIndent GT ref pos
              | pos == lvl -> p
              | otherwise  -> incorrectIndent EQ lvl pos
      xs  <- indentedItems ref lvl sc p
      f (x:xs)

indentedItems :: MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = go
  where
    go = do
      sc
      pos  <- indentLevel
      done <- isJust <$> optional eof
      if done
        then return []
        else if | pos <= ref -> return []
                | pos == lvl -> (:) <$> p <*> go
                | otherwise  -> incorrectIndent EQ lvl pos
