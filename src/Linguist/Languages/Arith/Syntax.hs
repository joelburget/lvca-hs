{-# LANGUAGE QuasiQuotes #-}

module Linguist.Languages.Arith.Syntax where

import           Data.Text         (Text)
import           NeatInterpolation

syntaxT :: Text
syntaxT = [text|
Arith ::=
  Add(Arith; Arith)
  Sub(Arith; Arith)
  Mul(Arith; Arith)
  // note: skipping division because it's hard to implement using peano
  // arithmetic
  Const[Integer]
  |]

-- Meaning in terms of add, sub, and mul primitives.
--
-- Due to using machine primitives this has an operational feel because we need
-- to evaluate terms before handing them to primitives.
machineDynamicsT :: Text
machineDynamicsT = [text|
  [[ Add(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp(prim:add; a'; b')))
  [[ Sub(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp(prim:sub; a'; b')))
  [[ Mul(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp(prim:mul; a'; b')))
  [[ Const(i)  ]] = i
  |]

-- Meaning in terms of peano arithmetic.
peanoDynamicsT :: Text
peanoDynamicsT = [text|
  [[ Add(a; b) ]] = ZElim([[ a ]];
    // Z:
    [[ b ]];
    // S aPred:
    aPred. S([[ Add(aPred; b) ]])
    )

  // Rec as defined in pfpl section 9.1
  // starting from `a`, fold `b` times, subtracting one each time
  [[ Sub(a; b) ]] = Rec(
    [[ b ]];
    [[ a ]];
    _. acc. ZElim(acc; Z; accPred. accPred)
    )

  // starting from Z, fold `b` times, adding `a` each time
  [[ Mul(a; b) ]] = Rec(
    [[ b ]];
    Z;
    _. acc. Rec(
      [[ a ]];
      acc;
      _. acc'. S(acc')
      )
    )

  [[ Const(0) ]] = Z
  [[ Const(i) ]] = S([[ prim:sub(i, 1) ]])
  |]
