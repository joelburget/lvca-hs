{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Lvca.Languages.Arith where

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
import           Text.Megaparsec
  (ParseErrorBundle, runParser, choice, errorBundlePretty)
import           NeatInterpolation

import           Lvca.FunctorUtil
import           Lvca.Languages.Arith.Syntax    ()
import           Lvca.ParseDenotationChart      (parseDenotationChart)
import qualified Lvca.ParseDenotationChart      as PD
import           Lvca.ParseUtil
import           Lvca.Types
import           Lvca.TH
import           Lvca.ParseLanguage


newtype E = E { unE :: Either Int Text }
  deriving (Eq, Show)

makeWrapped ''E

instance AsFacet Text E where
  facet = _Wrapped . _Right

instance Pretty E where
  pretty = unE >>> \case
    Left  i -> pretty i
    Right t -> "\"" <> pretty t <> "\""

mkTypes (Options "Arith" (Just "syntax") Map.empty)
  "Arith ::=                                                                \n\
  \  Add(Arith; Arith)                                                      \n\
  \  Sub(Arith; Arith)                                                      \n\
  \  Mul(Arith; Arith)                                                      \n\
  \  // note: skipping division because it's hard to implement using peano  \n\
  \  // numbers                                                             \n\
  \  Z                                                                      \n\
  \  S(Arith)"
mkSyntaxInstances ''Arith

codomainT1 :: Text
codomainT1 = "Int ::= {Int}"

codomainT2 :: Text
codomainT2 = [text|
  Int ::=
    Z()
    S(Int)
    Rec(Int; Int; Int. Int. Int)
  |]

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left <$> intLiteral
  , Right "add" <$ symbol "add"
  , Right "sub" <$ symbol "sub"
  , Right "mul" <$ symbol "mul"
  ]

-- Meaning of terms with int externals in terms of add, sub, and mul
-- primitives.
--
-- Due to using machine primitives this has an operational feel because we need
-- to evaluate terms before handing them to primitives.
machineDynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart Void (Either Text E))
machineDynamics = runParser (parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)"
  [text|
  [[ Add(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp({add}; a'; b')))
  [[ Sub(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp({sub}; a'; b')))
  [[ Mul(a; b) ]] = Eval([[ a ]]; a'.
                      Eval([[ b ]]; b'.
                        PrimApp({mul}; a'; b')))
  [[ Z()       ]] = Value(Int{0})
  [[ S(a)      ]] = Eval([[ a ]]; a'. PrimApp({add}; a'; Int{1}))
  |]


peanoDynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart Void (Either Text Void))
peanoDynamics = runParser (parseDenotationChart noParse noParse)
  "(arith peano dynamics)"
  [text|
  // Rec as defined in pfpl section 9.1
  // starting from `a`, fold `b` times, adding one each time
  [[ Add(a; b) ]] = Rec(
    [[ b ]];
    [[ a ]];
    _. acc. S(acc)
  )

  // starting from `a`, fold `b` times, subtracting one each time
  [[ Sub(a; b) ]] = Rec(
    [[ b ]];
    [[ a ]];
    _. acc. Rec([[ acc ]]; Z; accPred. _. accPred)
    )

  // starting from Z, fold `b` times, adding `a` each time
  [[ Mul(a; b) ]] = Rec(
    [[ b ]];
    Z;
    _. acc. Rec(
      [[ a ]];
      acc;
      _. acc'. S(acc')
      )
    )

  // TODO: i'd like to write `Z` but then we need a way to disambiguate from a
  // var
  [[ Z    ]] = Z()
  [[ S(a) ]] = S([[ a ]])
  |]

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
