{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies     #-}

module Linguist.Languages.Arith where

import Control.Monad.Reader (runReader)
import Control.Lens (pattern Empty, pattern (:<), _Right, _Wrapped)
import Control.Applicative ((<$))
import Data.Text (Text)
import Data.Sequence (Seq)
import           Control.Lens.TH
import           Data.Void                          (Void)
import           Text.Megaparsec                    (ParseErrorBundle, runParser, choice, runParserT, errorBundlePretty)
import Text.Megaparsec.Char.Lexer (decimal)
import EasyTest
import           Data.Text.Prettyprint.Doc (Pretty(pretty), viaShow)

import           Linguist.ParseDenotationChart      (Parser, parseDenotationChart)
import           Linguist.ParseSyntaxDescription    (parseSyntaxDescription)
import           Linguist.ParseUtil
import           Linguist.Types                     -- (SyntaxChart, Term (..))
import Linguist.Util (forceRight)
import Linguist.Languages.Arith.Syntax
import Linguist.Proceed

import qualified Data.Map as Map
import           Linguist.TH
import Linguist.ParseLanguage (standardParser)

import Language.Haskell.TH (lookupTypeName, Type(..))

import Data.Diverse.Lens.Which

newtype E = E { unE :: Either Int Text }
  deriving (Eq, Show)

makeWrapped ''E

instance AsFacet Text E where
  facet = _Wrapped . _Right

instance Pretty E where
  pretty = viaShow

$(do
  Just t <- lookupTypeName "Integer"
  mkTypes syntaxT $ Map.singleton "Integer" $ PromotedT t)

makeLenses ''Arith

syntax :: Either (ParseErrorBundle Text Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(arith syntax)" syntaxT

machineDynamics :: Either (ParseErrorBundle Text Void) (DenotationChart Int E)
machineDynamics = runParser (parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" machineDynamicsT

parsePrim :: Parser E
parsePrim = E <$> choice
  [ Left <$> decimal
  , Right "add" <$ symbol "add"
  , Right "sub" <$ symbol "sub"
  , Right "mul" <$ symbol "mul"
  ]

peanoDynamics
  :: AsFacet Text a
  => Either (ParseErrorBundle Text Void) (DenotationChart Int a)
peanoDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith peano dynamics)" peanoDynamicsT

tm :: Term Int
tm = Term "Add"
  [ Term "Mul"
    [ PrimValue 1
    , Term "Sub"
      [ PrimValue 500
      , PrimValue 498
      ]
    ]
  , PrimValue 3
  ]

tm' :: Term Int
tm' =
  let parser = standardParser undefined
      env = (forceRight syntax, "Arith")
      exampleTerm :: Text
      exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
  in case runReader (runParserT parser "(example term)" exampleTerm) env of
       Left err   -> error $ errorBundlePretty err
       Right tm'' -> tm''

pattern PrimInt :: Int -> Term E
pattern PrimInt i = PrimValue (E (Left i))

evalMachinePrimitive :: Text -> Maybe (Seq (Term E) -> Term E)
evalMachinePrimitive = \case
  "add" -> Just $ \case
    PrimInt x :< PrimInt y :< Empty -> PrimInt (x + y)
    args -> error $ "bad call to add: " ++ show args
  "sub" -> Just $ \case
    PrimInt x :< PrimInt y :< Empty -> PrimInt (x - y)
    args -> error $ "bad call to sub: " ++ show args
  "mul" -> Just $ \case
    PrimInt x :< PrimInt y :< Empty -> PrimInt (x * y)
    args -> error $ "bad call to mul: " ++ show args
  _ -> Nothing

eval' :: Term Int -> Either String (Term E)
eval' = eval $ mkEvalEnv "Arith" (forceRight syntax)
  (forceRight machineDynamics)
  evalMachinePrimitive
  (Just . E . Left)

evalTests :: Test ()
evalTests = tests
  [ expectEq (eval' tm) (Right (PrimInt 5))
  ]
