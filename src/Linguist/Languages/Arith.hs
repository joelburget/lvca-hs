{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Linguist.Languages.Arith where

import           Control.Applicative                ((<$))
import           Control.Arrow                      ((>>>))
import           Control.Monad.Reader               (runReaderT)
import           Control.Lens                       (pattern Empty, pattern (:<), _Right, _Wrapped)
import           Control.Lens.TH
import           Data.Diverse.Lens.Which
import qualified Data.Map                           as Map
import           Data.Sequence                      (Seq)
import           Data.Text                          (Text)
import           Data.Text.Prettyprint.Doc          (Pretty(pretty))
import           Data.Void                          (Void)
import           EasyTest
import           Language.Haskell.TH                (lookupTypeName, Type(..))
import           Text.Megaparsec                    (ParseErrorBundle, runParser, choice, errorBundlePretty)

import           Linguist.Languages.Arith.Syntax
import           Linguist.ParseDenotationChart      (parseDenotationChart)
import qualified Linguist.ParseDenotationChart      as PD
import           Linguist.ParseSyntaxDescription    (parseSyntaxDescription)
import           Linguist.ParseUtil
import           Linguist.Proceed
import           Linguist.Types
import           Linguist.Util                      (forceRight)
import           Linguist.TH
import           Linguist.ParseLanguage


newtype E = E { unE :: Either Int Text }
  deriving (Eq, Show)

makeWrapped ''E

instance AsFacet Text E where
  facet = _Wrapped . _Right

instance Pretty E where
  pretty = unE >>> \case
    Left  i -> pretty i
    Right t -> "\"" <> pretty t <> "\""

$(mkTypes domainT Map.empty)
  -- Just t <- lookupTypeName "Text"
  -- Just i <- lookupTypeName "Int"
  -- mkTypes domainT $ Map.fromList
  --   [ ("Prim", PromotedT t)
  --   , ("Int",  PromotedT i)
  --   ])

makeLenses ''Arith

syntax :: Either (ParseErrorBundle Text Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(arith syntax)" domainT

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left <$> intLiteral
  , Right "add" <$ symbol "add"
  , Right "sub" <$ symbol "sub"
  , Right "mul" <$ symbol "mul"
  ]

-- machineDynamics
--   :: Either (ParseErrorBundle Text Void) (DenotationChart Void Int)
-- machineDynamics = runParser (parseDenotationChart noParse parsePrim)
--   "(arith machine dynamics)" machineDynamicsT

peanoDynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart Void Void)
peanoDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith peano dynamics)" peanoDynamicsT

pattern S' x = Term "S" [ x ]
pattern Z'   = Term "Z" [   ]

example :: Term Void
example = Term "Add"
  [ Term "Mul"
    [ Term "Int" [ S' Z' ]
    , Term "Sub"
      [ Term "Int" [ S' (S' Z') ]
      , Term "Int" [ S' Z' ]
      ]
    ]
  , Term "Int" [ S' (S' (S' Z')) ]
  ]

tm' :: Term E
tm' =
  let env = ParseEnv (forceRight syntax) "Arith" UntaggedExternals primParsers
      parse = runReaderT standardParser env
      exampleTerm :: Text
      exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
  in case runParser parse "(example term)" exampleTerm of
       Left err   -> error $ errorBundlePretty err
       Right tm'' -> tm''

pattern PrimInt :: Int -> Term E
pattern PrimInt i = Term "Int" [ PrimValue (E (Left i)) ]

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

-- machineEval :: Term Void -> Either String (Term E)
-- machineEval = eval $ mkEvalEnv "Arith" (forceRight syntax)
--   (forceRight machineDynamics)
--   evalMachinePrimitive
--   (Just . E . Left)

peanoEval :: Term Void -> Either String (Term Void)
peanoEval = eval $ mkEvalEnv "Arith" (forceRight syntax)
  (forceRight peanoDynamics)
  (const Nothing)
  Just

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Int" , E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Prim", E . Right <$> stringLiteral)
  ]

arithTests :: Test ()
arithTests = tests
  -- [ scope "eval" $ expectEq (machineEval example) (Right (PrimInt 5))
  [ scope "prop_parse_pretty" $
    testProperty $ prop_parse_pretty (forceRight syntax) "Arith"
      (const Nothing) primParsers
  , scope "prop_serialise_identity" $ testProperty $
    prop_serialise_identity @() (forceRight syntax) "Arith" (const Nothing)
  , scope "parse" $ parseTest
      (ParseEnv (forceRight syntax) "Arith" UntaggedExternals primParsers)
      "Za"
      (Var "Za")
  ]
