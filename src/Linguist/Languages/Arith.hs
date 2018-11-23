{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies     #-}

module Linguist.Languages.Arith where

import Control.Applicative ((<|>))
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

import           Linguist.Languages.Arith.Syntax
import           Linguist.ParseDenotationChart      (parseDenotationChart)
import qualified Linguist.ParseDenotationChart      as PD
import qualified Linguist.ParseLanguage             as PL
import           Linguist.ParseSyntaxDescription    (parseSyntaxDescription)
import           Linguist.ParseUtil
import           Linguist.Proceed
import           Linguist.Types                     -- (SyntaxChart, Term (..))
import           Linguist.Util                      (forceRight)

import qualified Data.Map as Map
import           Linguist.TH
import Linguist.ParseLanguage (Parser, standardParser, prop_parse_pretty)

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
  in case runParser (runReaderT parser env) "(example term)" exampleTerm of
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

arithTests :: Test ()
arithTests = tests
  [ scope "eval" $ tests
    [ expectEq (eval' tm) (Right (PrimInt 5))
    ]
  , scope "prop" $ tests
    [ testProperty $ prop_parse_pretty (forceRight syntax) "Arith"
      (const Nothing) (undefined :: PL.Parser ())
    ]
  , let primParser =
          E . Left  <$> (intLiteral :: Parser Int) <|>
          E . Right <$> stringLiteral
        parser = standardParser primParser
        runP str = (runParser (runReaderT parser (forceRight syntax, "Arith"))
          "(test)" str)
        expectParse str tm = scope (Text.unpack str) $ case runP str of
          Left err       -> fail $ errorBundlePretty err
          Right parsedTm -> expectEq parsedTm tm
        expectNoParse str = scope (Text.unpack str) $ case runP str of
          Left _   -> ok
          Right tm -> fail $ "parsed " ++ show tm
    in scope "parse" $ tests
    [ expectParse
        "Za"
        (Var "Za")
    ]
  ]
