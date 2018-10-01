{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Linguist.Languages.Arith where

import Control.Lens (pattern Empty, pattern (:<))
import Data.Text (Text)
import Data.Sequence (Seq)
import           Control.Lens.TH
import           Data.Void                          (Void)
import           Text.Megaparsec                    (ParseErrorBundle, runParser)
import EasyTest

import           Linguist.ParseDenotationChart
import           Linguist.ParseSyntaxDescription
import           Linguist.ParseUtil
import           Linguist.Types                     -- (SyntaxChart, Term (..))
import Linguist.Util (forceRight)
import Linguist.Languages.Arith.Syntax
import Linguist.Proceed (eval)

import qualified Data.Map as Map
import           Linguist.TH
import Language.Haskell.TH (lookupTypeName, Type(..))

import Debug.Trace

$(do
  Just t <- lookupTypeName "Integer"
  mkTypes syntaxT $ Map.singleton "Integer" $ PromotedT t)

makeLenses ''Arith

syntax :: Either (ParseErrorBundle Text Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(arith syntax)" syntaxT

machineDynamics :: Either (ParseErrorBundle Text Void) (DenotationChart Int Text)
machineDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith machine dynamics)" machineDynamicsT

peanoDynamics :: Either (ParseErrorBundle Text Void) (DenotationChart Int a)
peanoDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith peano dynamics)" peanoDynamicsT

tm :: Term Int
tm = Term "Add"
  [ Term "Mul"
    [ PrimValue 1
    , PrimValue 2
    ]
  , PrimValue 3
  ]

evalMachinePrimitive :: Text -> Maybe (Seq (Term Int) -> Term Int)
evalMachinePrimitive = \case
  "add" -> Just $ \case
    PrimValue x :< PrimValue y :< Empty -> PrimValue (x + y)
    args -> error $ "bad call to add: " ++ show args
  "sub" -> Just $ \case
    PrimValue x :< PrimValue y :< Empty -> PrimValue (x - y)
    _ -> error "bad call to sub"
  "mul" -> Just $ \case
    PrimValue x :< PrimValue y :< Empty -> PrimValue (x * y)
    _ -> error "bad call to mul"
  _ -> Nothing

evalTests :: Test ()
evalTests =
  let eval' :: Term Int -> Either String (Term Int)
      eval' = eval "Arith" (forceRight syntax)
        (traceShowId $ forceRight machineDynamics)
        id
        evalMachinePrimitive
  in tests
       [ expect $ traceShowId (eval' tm) == Right (PrimValue 5)
       ]
