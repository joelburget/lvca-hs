{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies     #-}

module Linguist.Languages.Arith where

import Control.Monad.Reader (runReaderT)
import Control.Lens (pattern Empty, pattern (:<), _Right, _Wrapped)
import Control.Applicative ((<$))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Sequence (Seq)
import           Control.Lens.TH
import           Data.Void                          (Void)
import           Text.Megaparsec                    (ParseErrorBundle, runParser, choice, errorBundlePretty)
import           EasyTest
import           Data.Text.Prettyprint.Doc (Pretty(pretty), viaShow)
import qualified Data.Map as Map

import           Linguist.Languages.Arith.Syntax
import           Linguist.ParseDenotationChart      (parseDenotationChart)
import qualified Linguist.ParseDenotationChart      as PD
import           Linguist.ParseSyntaxDescription    (parseSyntaxDescription)
import           Linguist.ParseUtil
import           Linguist.Proceed
import           Linguist.Types                     -- (SyntaxChart, Term (..))
import           Linguist.Util                      (forceRight)
import           Linguist.TH
import           Linguist.ParseLanguage (Parser, ParseEnv(ParseEnv),
  standardParser, prop_parse_pretty, ExternalStyle(UntaggedExternals),
  ExternalParsers)

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
  Just t <- lookupTypeName "Text"
  Just i <- lookupTypeName "Int"
  mkTypes syntaxT $ Map.fromList
    [ ("Prim", PromotedT t)
    , ("Int", PromotedT i)
    ])

makeLenses ''Arith

syntax :: Either (ParseErrorBundle Text Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(arith syntax)" syntaxT

machineDynamics :: Either (ParseErrorBundle Text Void) (DenotationChart Int E)
machineDynamics = runParser (parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" machineDynamicsT

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left <$> intLiteral
  , Right "add" <$ symbol "add"
  , Right "sub" <$ symbol "sub"
  , Right "mul" <$ symbol "mul"
  ]

peanoDynamics
  :: AsFacet Text a
  => Either (ParseErrorBundle Text Void) (DenotationChart Int a)
peanoDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith peano dynamics)" peanoDynamicsT

example :: Term Int
example = Term "Add"
  [ Term "Mul"
    [ PrimValue "Int" 1
    , Term "Sub"
      [ PrimValue "Int" 500
      , PrimValue "Int" 498
      ]
    ]
  , PrimValue "Int" 3
  ]

tm' :: Term E
tm' =
  let parser = standardParser primParsers
      env = ParseEnv (forceRight syntax) "Arith" UntaggedExternals
      exampleTerm :: Text
      exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
  in case runParser (runReaderT parser env) "(example term)" exampleTerm of
       Left err   -> error $ errorBundlePretty err
       Right tm'' -> tm''

pattern PrimInt :: Int -> Term E
pattern PrimInt i = PrimValue "Int" (E (Left i))

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

primParsers :: ExternalParsers E
primParsers = Map.singleton "Arith" $ Map.fromList
  [ ("Int" , E . Left  <$> (intLiteral :: Parser Int))
  , ("Prim", E . Right <$> stringLiteral)
  ]

arithTests :: Test ()
arithTests = tests
  [ scope "eval" $ tests
    [ expectEq (eval' example) (Right (PrimInt 5))
    ]
  , scope "prop_parse_pretty" $ tests
    [ testProperty $ prop_parse_pretty (forceRight syntax) "Arith"
      (const Nothing) primParsers
    ]
  , let parser = standardParser primParsers
        parseEnv = ParseEnv (forceRight syntax) "Arith" UntaggedExternals
        runP str = runParser (runReaderT parser parseEnv) "(test)" str
        expectParse str tm = scope (Text.unpack str) $ case runP str of
          Left err       -> fail $ errorBundlePretty err
          Right parsedTm -> expectEq parsedTm tm
    in scope "parse" $ tests
    [ expectParse
        "Za"
        (Var "Za")
    ]
  ]
