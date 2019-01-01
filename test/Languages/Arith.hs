{-# language TypeApplications #-}
module Languages.Arith where

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

import           Lvca.FunctorUtil
import           Lvca.Languages.Arith.Syntax
import           Lvca.ParseDenotationChart      (parseDenotationChart)
import qualified Lvca.ParseDenotationChart      as PD
import           Lvca.ParseUtil
import           Lvca.Types
import           Lvca.TH
import           Lvca.ParseLanguage

import Lvca.Languages.Arith

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
