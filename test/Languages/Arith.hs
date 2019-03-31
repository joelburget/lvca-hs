{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Languages.Arith where

import           Control.Applicative       ((<$))
import           Control.Arrow             ((>>>))
import           Control.Lens
  (Prism', from, preview, prism', review, _Left, _Right, _Wrapped)
import           Control.Lens.TH
import           Control.Monad.Reader      (runReader, runReaderT)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.CPS
import           Data.Bifunctor            (bimap, second)
import           Data.Bitraversable
import           Data.Diverse.Lens.Which
import qualified Data.Map                  as Map
import           Data.Sequence             (Seq)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty(pretty))
import           Data.Void                 (Void)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec
  (ParseErrorBundle, choice, errorBundlePretty, parseTest, runParser)

import           Lvca
import           Test.ParseTerm
  (earleyConcreteParseTermTest, prop_parse_abstract_pretty,
  standardParseTermTest)
import           Test.Types                (prop_serialise_identity)

newtype E = E { unE :: Either Int Text }
  deriving (Eq, Show)

makeWrapped ''E

instance AsFacet Text E where
  facet = _Wrapped . _Right

instance Pretty E where
  pretty = unE >>> \case
    Left  i -> pretty i
    Right t -> "\"" <> pretty t <> "\""

mkTypes (Options (Just "syntax") Map.empty)
  [text|
  Arith ::=
    Add(Arith; Arith)
    Sub(Arith; Arith)
    Mul(Arith; Arith)
    // note: skipping division because it's hard to implement using peano
    // numbers
    Z
    S(Arith)
  |]
mkSyntaxInstances ''Arith

-- | Manually specified concrete syntax
concreteArith :: ConcreteSyntax
concreteArith = mkConcreteSyntax
  [ [ ConcreteSyntaxRule "Z" [] (MixfixDirective "Z") ]

  , [ ConcreteSyntaxRule "S" ["x"] $
        MixfixDirective $ "S" >>: space >>: SubTerm "x"
    ]

  , [ ConcreteSyntaxRule "Mul" [] (InfixDirective "*" Infixl) ]
  , [ ConcreteSyntaxRule "Add" [] (InfixDirective "+" Infixl)
    , ConcreteSyntaxRule "Sub" [] (InfixDirective "-" Infixl)
    ]
  ]

-- | This should be the same as the manually specified concrete syntax
concreteArith2 :: Either (ParseErrorBundle Text Void) ConcreteSyntax
concreteArith2 = runParser
  (parseConcreteSyntaxDescription
    (ParseEnv syntax "Arith" UntaggedExternals noExternalParsers))
  "(concreteArith2)" [text|
  concrete syntax:
    - Z()       ~ "Z";
    - S(x)      ~ "S " x;
    - Mul(x; y) ~ infixl x "*" y;
    - Add(x; y) ~ infixl x "+" y;
      Sub(x; y) ~ infixl x "-" y;
  |]

pt :: IO ()
pt = parseTest
  (fmap pretty $ parseConcreteSyntaxDescription
    (ParseEnv syntax "Arith" UntaggedExternals noExternalParsers))
  [text|
  concrete syntax:
    - Z()       ~ "Z";
    - S(x)      ~ "S" x;
    - Mul(x; y) ~ infixl x "*" y;
    - Add(x; y) ~ infixl x "+" y;
      Sub(x; y) ~ infixl x "-" y;
  |]

mkTypes defOptions
  [text|
  RecInt ::=
    Rec(RecInt; RecInt; RecInt. RecInt. RecInt)
    Zr()
    Sr(RecInt)
  |]
mkSyntaxInstances ''RecInt

{-
-- XXX Fix syntax generation when we create multiple data types
mkTypes (Options Nothing Map.empty)
  [text|
  Op ::=
    AddOp
    SubOp
    MulOp
    Push
    Succ

  Zero ::= Zero

  Program ::= Pgm(OpSeq)

  OpSeq ::=
    Nil
    Cons(Op; OpSeq)
  |]
mkSyntaxInstances ''Op
mkSyntaxInstances ''Zero
mkSyntaxInstances ''Program
mkSyntaxInstances ''OpSeq
-}

stackMachineDenotation :: DenotationChart a (Either Text b)
Right stackMachineDenotation = runParser (parseDenotationChart noParse noParse)
  "(stack machine dynamics)"
  [text|
  [[ Add(a; b) ]] = Cons([[ a ]]; Cons([[ b ]]; Cons(Add(); Nil())))
  [[ Sub(a; b) ]] = Cons([[ a ]]; Cons([[ b ]]; Cons(Sub(); Nil())))
  [[ Mul(a; b) ]] = Cons([[ a ]]; Cons([[ b ]]; Cons(Mul(); Nil())))
  [[ Z         ]] = Zero()
  [[ S(a)      ]] = Cons([[ a ]]; Cons(Succ; Nil()))
  |]

codomainT1 :: Text
codomainT1 = "Int ::= {Int}"

parsePrim :: DenotationChartParser E
parsePrim = E <$> choice
  [ Left <$> intLiteral
  , Right "add" <$ symbol "add"
  , Right "sub" <$ symbol "sub"
  , Right "mul" <$ symbol "mul"
  ]

peanoDynamics :: Either
  (ParseErrorBundle Text Void)
  (DenotationChart Text (Either Text Void))
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
  [[ Z()  ]] = Z()
  [[ S(a) ]] = S([[ a ]])
  |]

pattern S' :: Term a -> Term a
pattern Z' ::           Term a

pattern S' x = Term "S" [ x ]
pattern Z'   = Term "Z" [   ]

addOneOne :: Term Void
addOneOne = Term "Add" [ S' Z', S' Z' ]

addAssoc :: Term Void
addAssoc = Term "Add"
  [ Term "Add" [ Z', Z' ]
  , Term "Add" [ Z', Z' ]
  ]

exampleTm :: Term Void
exampleTm = Term "Add"
  [ Term "Mul"
    [ S' Z'
    , Term "Sub"
      [ S' (S' Z')
      , S' Z'
      ]
    ]
  , S' (S' (S' Z'))
  ]

pattern PrimInt :: Int -> Term E
pattern PrimInt i = Term "Int" [ PrimValue (E (Left i)) ]

arithTests :: Test
arithTests = tests
  [ scope "prop_parse_abstract_pretty" $
    prop_parse_abstract_pretty syntax "Arith"
      (const Nothing) primParsers

--   TODO: this fails because the syntax can't parse variables
--   , scope "prop_parse_concrete_pretty" $
--     property $ prop_parse_concrete_pretty syntax "Arith" concreteArith

  , scope "prop_serialise_identity" $
    prop_serialise_identity @() syntax "Arith" (const Nothing)
  , scope "standard parsing" $ standardParseTermTest
      (ParseEnv syntax "Arith" UntaggedExternals primParsers)
      "Za"
      (Var "Za")
  , scope "pretty-printing" $
    let expectEq1 tm str = unitTest $ do
          let result = show $ runReader (prettyTm tm) (-1, concreteArith)
          result === str
        expectEq2 tm str = unitTest $ do
          let result = show $ runReader (prettyTm tm) (-1, forceRight concreteArith2)
          result === str
    in tests
         [ addOneOne `expectEq1` "S Z + S Z"
         , addAssoc  `expectEq1` "Z + Z + (Z + Z)"
         , exampleTm `expectEq1` "S Z * (S (S Z) - S Z) + S (S (S Z))"
         , addOneOne `expectEq2` "S Z + S Z"
         , addAssoc  `expectEq2` "Z + Z + (Z + Z)"
         , exampleTm `expectEq2` "S Z * (S (S Z) - S Z) + S (S (S Z))"
         ]

  , scope "concrete parsing with earley" $
    let expectEq1 str tm = earleyConcreteParseTermTest concreteArith str tm
        expectEq2 str tm = earleyConcreteParseTermTest (forceRight concreteArith2) str tm
    in tests
         [ "S Z + S Z"                           `expectEq1` addOneOne
         , "Z + Z + (Z + Z)"                     `expectEq1` addAssoc
         , "S Z * (S (S Z) - S Z) + S (S (S Z))" `expectEq1` exampleTm
         , "S Z + S Z"                           `expectEq2` addOneOne
         , "Z + Z + (Z + Z)"                     `expectEq2` addAssoc
         , "S Z * (S (S Z) - S Z) + S (S (S Z))" `expectEq2` exampleTm
         ]
  ]
  where

    primParsers :: ExternalParsers E
    primParsers = makeExternalParsers
      [ ("Int" , E . Left  <$> (intLiteral :: ExternalParser Int))
      , ("Prim", E . Right <$> stringLiteral)
      ]

    _example2 :: Term E
    _example2 =
      let env = ParseEnv syntax "Arith" UntaggedExternals primParsers
          parse = runReaderT standardParser env
          exampleTerm :: Text
          exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
      in case runParser parse "(example term)" exampleTerm of
           Left err   -> error $ errorBundlePretty err
           Right tm'' -> tm''
