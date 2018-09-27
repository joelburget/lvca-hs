{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Linguist.Languages.Arith where

import           Control.Lens.TH
import           Data.Void                          (Void)
import           Text.Megaparsec                    (ParseError, runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types                     (SyntaxChart, Term (..))

import Linguist.Languages.Arith.Syntax

import qualified Data.Map as Map
import           Linguist.TH
import Language.Haskell.TH (lookupTypeName, Type(..), mkName)

syntax' :: Either (ParseError Char Void) SyntaxChart
syntax' = runParser parseSyntaxDescription "(document syntax)" syntax

$(do
  Just t <- lookupTypeName "Integer"
  mkTypes syntax $ Map.singleton "Integer" $ PromotedT t)

makeLenses ''Arith
