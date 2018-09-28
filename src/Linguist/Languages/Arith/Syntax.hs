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
  Div(Arith; Arith)
  Const[Integer]
  |]

dynamicsT :: Text
dynamicsT = [text|
  [[ Add(a; b) ]] = PrimAdd([[ a ]], [[ b ]])
  [[ Sub(a; b) ]] = PrimSub([[ a ]], [[ b ]])
  [[ Mul(a; b) ]] = PrimMul([[ a ]], [[ b ]])
  [[ Div(a; b) ]] = PrimDiv([[ a ]], [[ b ]])
  [[ Const(i)  ]] = Const(i)
  |]
