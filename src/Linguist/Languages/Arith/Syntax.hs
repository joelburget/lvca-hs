{-# LANGUAGE QuasiQuotes #-}

module Linguist.Languages.Arith.Syntax where

import           Data.Text         (Text)
import           NeatInterpolation

domainT :: Text
domainT = [text|
Arith ::=
  Add(Arith; Arith)
  Sub(Arith; Arith)
  Mul(Arith; Arith)
  // note: skipping division because it's hard to implement using peano
  // numbers
  Z
  S(Arith)
  |]

codomainT1 :: Text
codomainT1 = "Int ::= {Int}"

codomainT2 :: Text
codomainT2 = [text|
  Int ::=
    Z()
    Z(Int)
  |]

-- Meaning of terms with int externals in terms of add, sub, and mul
-- primitives.
--
-- Due to using machine primitives this has an operational feel because we need
-- to evaluate terms before handing them to primitives.
machineDynamicsT :: Text
machineDynamicsT = [text|
  [[ Add(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp({add}; a'; b')))
  [[ Sub(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp({sub}; a'; b')))
  [[ Mul(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp({mul}; a'; b')))
  [[ Z()       ]] = Value(Int{0})
  [[ S(a)      ]] = PrimApp({add}; [[ a ]]; Int{1})
  [[ Int(i)    ]] = Value(Int([[ i ]]))
  // Int(1) -> Int(1)
  |]

-- Meaning in terms of peano numbers.
peanoDynamicsT :: Text
peanoDynamicsT = [text|
  // Rec as defined in pfpl section 9.1
  // starting from `a`, fold `b` times, adding one each time
  [[ Add(a; b) ]] = Rec(
    [[ b ]];
    [[ a ]];
    _. acc. S(acc)
  )

  // starting from `a`, fold `b` times, subtracting one each time
  [[ Sub(a; b) ]] = Rec(
    [[ b ]];
    [[ a ]];
    _. acc. Rec([[ acc ]]; Z; accPred. _. accPred)
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

  // TODO: i'd like to write `Z` but then we need a way to disambiguate from a
  // var
  [[ Z    ]] = Z()
  [[ S(a) ]] = S([[ a ]])
  |]
