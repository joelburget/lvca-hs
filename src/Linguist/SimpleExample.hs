{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.SimpleExample where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
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

type T = Either Int Text

tm1, tm2 :: Term T
tm1 = Term "let"
  [ Var "x"
  , PrimTerm (Left 1)
  , Term "plus"
    [ Var "x"
    , PrimTerm (Left 2)
    ]
  ]
tm2 = Term "cat"
  [ PrimTerm (Right "foo")
  , PrimTerm (Right "bar")
  ]

pattern VI :: Int -> Value T
pattern VI x = PrimValue (Left x)

pattern VS :: Text -> Value T
pattern VS x = PrimValue (Right x)

denotation :: DenotationChart T
denotation = DenotationChart $ Map.fromList
  [ ("var", Variable)
  , ("num", Primitive (SomeTypeRep (typeRep @Int)))
  , ("str", Primitive (SomeTypeRep (typeRep @Text)))
  , ("plus", CallForeign $ \case
    VI x :< VI y :< Empty -> VI (x + y)
    _ -> error "TODO")
  , ("times", CallForeign $ \case
    VI x :< VI y :< Empty -> VI (x * y)
    _ -> error "TODO")
  , ("cat", CallForeign $ \case
    VS x :< VS y :< Empty -> VS (x <> y)
    _ -> error "TODO")
  , ("len", CallForeign $ \case
    VS x :< Empty -> VI (Text.length x)
    _ -> error "TODO")
  , ("let", BindIn 0 1 2)
  ]

proceed :: DenotationChart T -> StateStep T -> StateStep T
proceed (DenotationChart chart) (StateStep stack tm) = case tm of
  Term name subterms -> case chart ^. at name of
    Just (CallForeign f) -> case subterms of
      tm':tms -> StateStep
        (CbvFrame name Empty tms f : stack)
        tm'
      _ -> Errored "1"

    Just (BindIn nameSlot fromSlot toSlot) -> fromMaybe (Errored "BindIn") $ do
      Var name' <- subterms ^? ix nameSlot
      from'     <- subterms ^? ix fromSlot
      to'       <- subterms ^? ix toSlot
      let frame = BindingFrame $ Map.singleton name' (Left from')
      Just $ StateStep (frame : stack) to'
    Just _ -> Errored "3"
    Nothing -> Errored "4"

  Var name -> case findBinding stack name of
    Just (Left tm')  -> StateStep stack tm'
    Just (Right val) -> StateStep stack (Return val)
    Nothing    -> Errored "5"

  PrimTerm primTm -> case stack of
    CbvFrame _ vals [] f : stack' ->
      case f (vals |> PrimValue primTm) of
        result -> StateStep stack' (Return result)
    CbvFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvFrame name (vals |> PrimValue primTm) tms denote : stack')
      tm'
    BindingFrame  _ : stack' -> StateStep stack' tm
    [] -> Errored "empty stack with term"

  Return val -> case stack of
    [] -> Done val
    CbvFrame _name vals []       denote : stack' -> StateStep
      stack'
      (Return (denote (vals |> val)))
    CbvFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvFrame name (vals |> val) tms denote : stack')
      tm'
    BindingFrame _ : stack' -> StateStep stack' tm

proceed _ e@Errored{} = e
proceed _ d@Done{} = d
