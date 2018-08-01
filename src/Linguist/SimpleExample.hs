{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.SimpleExample where

import           Control.Lens
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Type.Reflection

import Linguist.Types

eChart :: SyntaxChart
eChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    -- TODO: is this the correct arity (sort)?
    [ Operator "num" (Arity' []) "numbers"
    , Operator "str" (Arity' []) "strings"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "var" (VariableArity "x") "variable"
    , Operator "num" (External "nat") "numeral"
    , Operator "str" (External "str") "literal"
    , Operator "plus" (Arity' [Valence [] "Exp", Valence [] "Exp"]) "addition"
    , Operator "times" (Arity' [Valence [] "Exp", Valence [] "Exp"]) "multiplication"
    , Operator "cat" (Arity' [Valence [] "Exp", Valence [] "Exp"]) "concatenation"
    , Operator "len" (Arity' [Valence [] "Exp"]) "length"
    -- TODO:
    -- * the book specifies this arity as
    --   - `let(e1;x.e2)`
    --   - `(Exp, Exp.Exp)Exp`
    -- * is it known that the `x` binds `e1`?
    -- * where is `x` specified?
    , Operator "let" (Arity' [Valence [] "Exp", Valence ["Exp"] "Exp"]) "definition"
    ])
  ]

tm1, tm2 :: Term T
tm1 = Term "plus"
  [ PrimTerm (Left 1)
  , Term "plus"
    [ PrimTerm (Left 2)
    , PrimTerm (Left 3)
    ]
  ]
tm2 = Term "cat"
  [ PrimTerm (Right "foo")
  , PrimTerm (Right "bar")
  ]

denotation :: DenotationChart T
denotation = DenotationChart $ Map.fromList
  [ ("var", Variable)
  , ("num", Foreign (SomeTypeRep (typeRep @Int)))
  , ("str", Foreign (SomeTypeRep (typeRep @Text)))
  , ("plus", CBV $ \[PrimValue (Left x), PrimValue (Left y)] -> PrimValue (Left (x + y)))
  , ("times", CBV $ \[PrimValue (Left x), PrimValue (Left y)] -> PrimValue (Left (x * y)))
  , ("cat", CBV $ \[PrimValue (Right x), PrimValue (Right y)] -> PrimValue (Right (x <> y)))
  , ("len", CBV $ \[PrimValue (Right x)] -> PrimValue (Left (Text.length x)))
  , ("let", Substitute 0 1)
  ]

proceed' :: DenotationChart T -> StateStep T -> StateStep T
proceed' (DenotationChart chart) (StateStep stack (Right tm)) = case tm of
  Term name subterms -> case chart ^. at name of
    Just (CBV f) -> case subterms of
      tm':tms -> StateStep
        (CbvFrame name [] tms f : stack)
        (Right tm')
      _ -> Errored "1"

    Just (Substitute _ _) -> undefined
    Just _ -> Errored "3"
    Nothing -> Errored "4"

  Var name -> case findBinding stack name of
    Just tmVal -> StateStep stack tmVal
    Nothing    -> Errored "5"

  PrimTerm primTm -> case stack of
    CbvFrame _ vals [] f : stack' ->
      case f (PrimValue primTm:vals) of
        result -> StateStep stack' (Left result)
    CbvFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvFrame name (PrimValue primTm : vals) tms denote : stack')
      (Right tm')
    ValueBindingFrame _ : stack' -> StateStep stack' (Left (PrimValue primTm))
    TermBindingFrame  _ : stack' -> StateStep stack' (Left (PrimValue primTm))
    [] -> Errored "empty stack with term"

proceed' _chart (StateStep stack (Left val)) =
  case stack of
    [] -> Done val
    CbvFrame _name vals []        denote : stack' -> StateStep
      stack'
      (Left (denote (val:vals)))
    CbvFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvFrame name (val:vals) tms denote : stack')
      (Right tm')
    ValueBindingFrame _ : stack' -> StateStep stack' (Left val)
    TermBindingFrame  _ : stack' -> StateStep stack' (Left val)

proceed' _ e@Errored{} = e
proceed' _ d@Done{} = d
