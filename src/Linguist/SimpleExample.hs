{-# LANGUAGE OverloadedStrings #-}
module Linguist.SimpleExample where

import           Control.Lens
import           Data.List       (findIndex)
import qualified Data.Map.Strict as Map

import Linguist.Types

eChart :: SyntaxChart
eChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    -- TODO: is this the correct arity (sort)?
    [ Operator "num" (Arity' []) "numbers"
    , Operator "str" (Arity' []) "strings"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "var" (Variable "x") "variable"
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

eTerm1, eTerm2 :: Term (Either Int String)
eTerm1 = Term "plus"
  [ Primitive (Left 1)
  , Term "plus"
    [ Primitive (Left 2)
    , Primitive (Left 3)
    ]
  ]
eTerm2 = Term "cat" [Primitive (Right "foo"), Primitive (Right "bar")]

isValue :: Term a -> Bool
isValue Primitive{} = True
isValue _ = False

proceed :: StateStep -> StateStep
proceed (StateStep ctx@(EvalContext stack vars) tm) = case tm of
  Term name subterms -> case findIndex (not . isValue) subterms of
    Just pos ->
      let (before, it:after) = splitAt pos subterms
          hterm = HoleyTerm name before after
      in StateStep (EvalContext (hterm:stack) vars) it
    Nothing ->
      let mResult = case tm of
            Term "plus" [Primitive (Left x), Primitive (Left y)]
              -> Just $ Primitive $ Left (x + y)
            Term "cat" [Primitive (Right x), Primitive (Right y)]
              -> Just $ Primitive $ Right (x ++ y)
            _ -> Nothing
      in case (mResult, stack) of
        (Nothing, _) -> Errored
        (Just result, []) -> Done result
        (Just result, frame:frames)
          -> StateStep (EvalContext frames vars) (fillHole frame result)
  Var name -> case vars ^. at name of
    Just tm' -> StateStep ctx tm'
    Nothing  -> Errored
  Primitive a -> StateStep ctx $ Primitive a -- complete
proceed Errored = Errored
proceed d@Done{} = d
