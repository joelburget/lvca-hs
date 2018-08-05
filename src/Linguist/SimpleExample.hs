{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.SimpleExample where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Linguist.Types

type E = Either Int Text

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

tm1, tm2 :: Term E
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

pattern VI :: Int -> Value E
pattern VI x = PrimValue (Left x)

pattern VS :: Text -> Value E
pattern VS x = PrimValue (Right x)

denotation :: DenotationChart E
denotation = DenotationChart
  [ (PatternVar "x", Variable "x")
  , (PatternPrim "num" "n", Value)
  , (PatternPrim "str" "s", Value)
  , (PatternTm "plus" [PatternPrim "num" "n_1", PatternPrim "num" "n_2"], CallForeign $ \case
    VI x :< VI y :< Empty -> VI (x + y)
    _ -> error "bad call to plus")
  , (PatternTm "times" [PatternPrim "num" "n_1", PatternPrim "num" "n_2"], CallForeign $ \case
    VI x :< VI y :< Empty -> VI (x * y)
    _ -> error "bad call to times")
  , (PatternTm "cat" [PatternPrim "str" "s_1", PatternPrim "str" "s_2"], CallForeign $ \case
    VS x :< VS y :< Empty -> VS (x <> y)
    _ -> error "bad call to cat")
  , (PatternTm "len" [PatternSort "e"], CallForeign $ \case
    VS x :< Empty -> VI (Text.length x)
    _ -> error "bad call to len")
  , (PatternTm "let" [PatternSort "e_1", BindingPattern "x" (PatternSort "e_2")],
    BindIn 0 1 2)
  ]

proceed :: DenotationChart E -> StateStep E -> StateStep E
proceed chart (StateStep stack tm) = case tm of
  Term name subterms -> case findMatch chart tm of
    Just (_assignment, CallForeign f) -> case subterms of
      tm':tms -> StateStep
        (CbvFrame name Empty tms f : stack)
        tm'
      _ -> Errored "1"

    Just (_assignment, BindIn nameSlot fromSlot toSlot) -> fromMaybe (Errored "BindIn") $ do
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
    Nothing          -> Errored "5"

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
