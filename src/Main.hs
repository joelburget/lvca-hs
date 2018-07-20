{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)

newtype SyntaxChart = SyntaxChart (Map Text Sort)

data Sort = Sort
  -- !Text       -- ^ name of the sort
  ![Text]     -- ^ set of variables
  ![Operator] -- ^ set of operators

data Operator = Operator
  !Text    -- ^ operator name
  !Arity
  !Text    -- ^ description

data Arity = Arity
  ![Valence] -- ^ the valences
  !Text      -- ^ the resulting sort

data Valence = Valence
  ![Text] -- ^ the sorts of all bound variables
  Text    -- ^ the resulting sort

eChart :: SyntaxChart
eChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    -- TODO: is this the correct arity (sort)?
    [ Operator "num" (Arity [] "t") "numbers"
    , Operator "str" (Arity [] "t") "strings"
    ])
  , ("Exp", Sort ["e"]
    -- TODO: is this the correct name and arity?
    [ Operator "var" (Arity [] "e") "variable"
    -- TODO:
    -- * do we have to really specify the resulting sort every time?
    -- * how do we refer to n, which is understood to be a number?
    , Operator "num" (Arity [Valence [] "n"] "e") "numeral"
    , Operator "str" (Arity [Valence [] "s"] "e") "literal"
    -- TODO: how do we give a unique name to these bound "e"s?
    , Operator "plus" (Arity [Valence [] "e", Valence [] "e"] "e") "addition"
    , Operator "times" (Arity [Valence [] "e", Valence [] "e"] "e") "multiplication"
    , Operator "cat" (Arity [Valence [] "e", Valence [] "e"] "e") "concatenation"
    , Operator "len" (Arity [Valence [] "e"] "e") "length"
    -- TODO:
    -- * the book specifies this arity as
    --   - `let(e1;x.e2)`
    --   - `(Exp, Exp.Exp)Exp`
    -- * is it known that the `x` binds `e1`?
    -- * where is `x` specified?
    , Operator "let" (Arity [Valence [] "e", Valence ["e"] "e"] "e") "definition"
    ])
  ]

main :: IO ()
main = putStrLn "Hello, Haskell!"
