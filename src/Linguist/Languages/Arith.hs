{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Linguist.Languages.Arith where

import           Control.Applicative                ((<$))
import           Control.Arrow                      ((>>>))
import           Control.Monad.Reader               (runReaderT)
import           Control.Lens
  (pattern Empty, pattern (:<), _Right, _Wrapped)
import           Control.Lens.TH
import           Data.Diverse.Lens.Which
import qualified Data.Map                           as Map
import           Data.Sequence                      (Seq)
import           Data.Text                          (Text)
import           Data.Text.Prettyprint.Doc          (Pretty(pretty))
import           Data.Void                          (Void)
import           EasyTest
import           Text.Megaparsec
  (ParseErrorBundle, runParser, choice, errorBundlePretty)

import           Linguist.FunctorUtil
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

$(mkTypes (Options "Arith" "syntax" Map.empty)
  "Arith ::=                                                               \n\
  \  Add(Arith; Arith)                                                     \n\
  \  Sub(Arith; Arith)                                                     \n\
  \  Mul(Arith; Arith)                                                     \n\
  \  // note: skipping division because it's hard to implement using peano \n\
  \  // numbers                                                            \n\
  \  Z                                                                     \n\
  \  S(Arith)")
mkSyntaxInstances ''Arith

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left <$> intLiteral
  , Right "add" <$ symbol "add"
  , Right "sub" <$ symbol "sub"
  , Right "mul" <$ symbol "mul"
  ]

machineDynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart Void (Either Text E))
machineDynamics = runParser (parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" machineDynamicsT

peanoDynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart Void (Either Text Void))
peanoDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith peano dynamics)" peanoDynamicsT

pattern S' :: Term a -> Term a
pattern Z' ::           Term a

pattern S' x = Fix (Term "S" [ x ])
pattern Z'   = Fix (Term "Z" [   ])

example :: Term Void
example = Fix $ Term "Add"
  [ Fix $ Term "Mul"
    [ S' Z'
    , Fix $ Term "Sub"
      [ S' (S' Z')
      , S' Z'
      ]
    ]
  , S' (S' (S' Z'))
  ]

tm' :: Term E
tm' =
  let env = ParseEnv syntax "Arith" UntaggedExternals primParsers
      parse = runReaderT standardParser env
      exampleTerm :: Text
      exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
  in case runParser parse "(example term)" exampleTerm of
       Left err   -> error $ errorBundlePretty err
       Right tm'' -> tm''

pattern PrimInt :: Int -> Term E
pattern PrimInt i = Fix (Term "Int" [ Fix (PrimValue (E (Left i))) ])

evalMachinePrimitive :: E -> Maybe (Seq (Term E) -> Term E)
evalMachinePrimitive (E (Right str)) = case str of
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
evalMachinePrimitive _ = Nothing

machineEval :: Term Void -> Either String (Term E)
machineEval = error "TODO"
-- eval $ mkEvalEnv "Arith" syntax
--   (forceRight machineDynamics)
--   evalMachinePrimitive
--   (const Nothing)

peanoEval :: Term Void -> Either String (Term Void)
peanoEval = error "TODO"
-- eval $ mkEvalEnv "Arith" syntax
--   (forceRight peanoDynamics)
--   (const Nothing)
--   (const Nothing)

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Int" , E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Prim", E . Right <$> stringLiteral)
  ]

arithTests :: Test ()
arithTests = tests
  [ scope "eval" $ expectEq (machineEval example) (Right (PrimInt 4))
  , scope "prop_parse_pretty" $
    testProperty $ prop_parse_pretty syntax "Arith"
      (const Nothing) primParsers
  , scope "prop_serialise_identity" $ testProperty $
    prop_serialise_identity @() syntax "Arith" (const Nothing)
  , scope "parse" $ parseTest
      (ParseEnv syntax "Arith" UntaggedExternals primParsers)
      "Za"
      (Fix (Var "Za"))
  ]
