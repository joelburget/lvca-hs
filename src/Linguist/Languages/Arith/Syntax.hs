{-# LANGUAGE QuasiQuotes #-}

module Linguist.Languages.Arith.Syntax where

import           Data.Text         (Text)
import           NeatInterpolation

syntax :: Text
syntax = [text|
Arith ::=
  Add(Arith; Arith)
  Sub(Arith; Arith)
  Mul(Arith; Arith)
  Div(Arith; Arith)
  Const[Integer]
  |]
