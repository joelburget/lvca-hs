{-# language QuasiQuotes      #-}
{-# language TemplateHaskell  #-}
{-# language TypeFamilies     #-}
{-# options_ghc -ddump-splices #-}
module Languages.Arith where

import           Control.Applicative                ((<$))
import           Control.Arrow                      ((>>>))
import           Control.Monad.Reader               (runReaderT)
import           Control.Lens
  (_Right, _Wrapped, preview, review, Prism', prism', _Left, from)
import           Control.Lens.TH
import           Data.Bifunctor                     (bimap, second)
import           Data.Bitraversable
import           Data.Diverse.Lens.Which
import qualified Data.Map                           as Map
import           Data.Sequence                      (Seq)
import           Data.Text                          (Text)
import           Data.Text.Prettyprint.Doc          (Pretty(pretty))
import           Data.Void                          (Void, absurd)
import           EasyTest
import           Text.Megaparsec
  (ParseErrorBundle, runParser, choice, errorBundlePretty)
import           NeatInterpolation

import Lvca

import Test.ParseLanguage
import qualified Text.Megaparsec as MP
import Test.Types

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
  "Arith ::=                                                                \n\
  \  Add(Arith; Arith)                                                      \n\
  \  Sub(Arith; Arith)                                                      \n\
  \  Mul(Arith; Arith)                                                      \n\
  \  // note: skipping division because it's hard to implement using peano  \n\
  \  // numbers                                                             \n\
  \  Z                                                                      \n\
  \  S(Arith)"
mkSyntaxInstances ''Arith

-- XXX Fix syntax generation when we create multiple data types
mkTypes (Options Nothing Map.empty)
  "Op ::=                                                                   \n\
  \  AddOp                                                                  \n\
  \  SubOp                                                                  \n\
  \  MulOp                                                                  \n\
  \  Push                                                                   \n\
  \  Succ                                                                   \n\
  \                                                                         \n\
  \Zero ::= Zero                                                            \n\
  \                                                                         \n\
  \Program ::= Program(List(Op))                                            \n\
  \                                                                         \n\
  \List a ::=                                                               \n\
  \  Nil                                                                    \n\
  \  Cons(a; List(a))"
mkSyntaxInstances ''StackMachine

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

codomainT2 :: Text
codomainT2 = [text|
  Int ::=
    Z()
    S(Int)
    Rec(Int; Int; Int. Int. Int)
  |]

parsePrim :: DenotationChartParser E
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
-- XXX
machineDynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart Text (Either Text E))
machineDynamics = runParser (parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)"
  [text|
  [[ Cons(a; Cons(b; Add)) ]] = Prim({add}; a; b)
  [[ Cons(a; Cons(b; Sub)) ]] = Prim({sub}; a; b)
  [[ Cons(a; Cons(b; Mul)) ]] = Prim({mul}; a; b)

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
  :: Either (ParseErrorBundle Text Void) (DenotationChart Text (Either Text Void))
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

pattern PrimInt :: Int -> Term E
pattern PrimInt i = Fix (Term "Int" [ Fix (PrimValue (E (Left i))) ])

upcast :: forall sub sup. Prism' sup sub -> Prism' (Term sup) (Term sub)
upcast p = prism' rtl ltr where
  rtl :: Term sub -> Term sup
  rtl (Fix tm) = Fix $ bimap (review p) rtl tm
  ltr :: Term sup -> Maybe (Term sub)
  ltr (Fix tm) = Fix <$> bitraverse (preview p) ltr tm

upcast' :: forall sub sup. (sub -> sup) -> Term sub -> Term sup
upcast' f (Fix tm) = Fix $ bimap f (upcast' f) tm

machineEval :: Term Void -> Either String (Term Int)
machineEval tm = case preview termP1 (upcast' @Void @Int absurd tm) of
  Nothing  -> Left "couldn't view term as Arith term"
  Just (tm' :: Fix (VarBindingF :+: Arith Int))
    -> case unMkDenotationChart patP termP2 stackMachineDenotation of
      Nothing    -> Left "couldn't unmake denotation chart"
      Just (chart :: DenotationChart' (Arith Text) (MachineF :+: Arith Text)) ->
        let resultTm :: (Either String (Fix (Arith Int)), Seq Text)
            resultTm = eval (EvalEnv Map.empty (const Nothing)) chart tm'
        in second (review termP3) $ fst resultTm

peanoEval :: Term Void -> Either String (Term Void)
peanoEval tm = case preview termP1 tm of
  Nothing  -> Left "couldn't view term as Arith term"
  Just tm' -> case unMkDenotationChart patP termP2' (forceRight peanoDynamics) of
    Nothing    -> Left "couldn't unmake denotation chart"
    Just chart ->
      let resultTm :: (Either String (Fix (Arith Void)), Seq Text)
          resultTm = eval (EvalEnv Map.empty (const Nothing)) chart tm'
      in second (review termP3) $ fst resultTm

patP :: Prism' (Pattern a) (Fix (PatVarF :+: Arith a))
patP = sumPrisms patVarP' (mkPatP patP)

termP1 :: Prism' (Term a) (Fix (VarBindingF :+: Arith a))
termP1 = sumPrisms (varBindingP termP1) (mkTermP termP1)

termP2 :: Prism'
  (Term (Either Text a))
  (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Arith Text))
termP2 = termAdaptor _Left . machineTermP (_Fix . _PrimValue)

termP3 :: Prism' (Term a) (Fix (Arith a))
termP3 = mkTermP termP3 . from _Fix

arithTests :: Test ()
arithTests = tests
  [ scope "eval" $ expectEq (machineEval example) $ Right $ Fix $ Term "foo" []
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
  where

    primParsers :: ExternalParsers E
    primParsers = makeExternalParsers
      [ ("Int" , E . Left  <$> (intLiteral :: ExternalParser Int))
      , ("Prim", E . Right <$> stringLiteral)
      ]

    example2 :: Term E
    example2 =
      let env = ParseEnv syntax "Arith" UntaggedExternals primParsers
          parse = runReaderT standardParser env
          exampleTerm :: Text
          exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
      in case runParser parse "(example term)" exampleTerm of
           Left err   -> error $ errorBundlePretty err
           Right tm'' -> tm''
