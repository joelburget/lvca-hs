{-# language QuasiQuotes      #-}
{-# language TemplateHaskell  #-}
{-# language TypeFamilies     #-}
module Languages.Arith where

import           Control.Applicative                ((<$))
import           Control.Arrow                      ((>>>))
import           Control.Monad.Reader               (runReaderT)
import           Control.Lens
  (_Right, _Wrapped, preview, review, Prism', prism', _Left)
import           Control.Lens.TH
import           Data.Bifunctor                     (bimap, second)
import           Data.Bitraversable
import           Data.Diverse.Lens.Which
import           Data.Foldable                      (asum)
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

example2 :: Term E
example2 =
  let env = ParseEnv syntax "Arith" UntaggedExternals primParsers
      parse = runReaderT standardParser env
      exampleTerm :: Text
      exampleTerm = "Add(Mul(1; Sub(500; 498)); 3)"
  in case runParser parse "(example term)" exampleTerm of
       Left err   -> error $ errorBundlePretty err
       Right tm'' -> tm''

pattern PrimInt :: Int -> Term E
pattern PrimInt i = Fix (Term "Int" [ Fix (PrimValue (E (Left i))) ])

type ArithI = Arith Int

data PrimOp a f -- error "TODO"

evalMachinePrimitive :: Text -> Maybe (Seq (ArithI (Fix ArithI)) -> ArithI (Fix ArithI))
evalMachinePrimitive = error "TODO"
  -- "add" -> Just $ \case
  --   PrimInt x :< PrimInt y :< Empty -> PrimInt (x + y)
  --   args -> error $ "bad call to add: " ++ show args
  -- "sub" -> Just $ \case
  --   PrimInt x :< PrimInt y :< Empty -> PrimInt (x - y)
  --   args -> error $ "bad call to sub: " ++ show args
  -- "mul" -> Just $ \case
  --   PrimInt x :< PrimInt y :< Empty -> PrimInt (x * y)
  --   args -> error $ "bad call to mul: " ++ show args
  -- _ -> Nothing

upcast :: forall sub sup. Prism' sup sub -> Prism' (Term sup) (Term sub)
upcast p = prism' rtl ltr where
  rtl :: Term sub -> Term sup
  rtl (Fix tm) = Fix $ bimap (review p) rtl tm
  ltr :: Term sup -> Maybe (Term sub)
  ltr (Fix tm) = Fix <$> bitraverse (preview p) ltr tm

upcast' :: forall sub sup. (sub -> sup) -> Term sub -> Term sup
upcast' f (Fix tm) = Fix $ bimap f (upcast' f) tm

machineEval :: Term Void -> Either String (Term Int)
machineEval tm =
  let tm' :: Fix (VarBindingF :+: Arith Int)
      Just tm' = preview termP1 (upcast' @Void @Int absurd tm)

  in case unMkDenotationChart patP' termP2 (forceRight machineDynamics) of
       Nothing -> Left "couldn't unmake denotation chart"
       Just chart ->
         let resultTm :: (Either String (Fix ArithI), Seq Text)
             resultTm = eval (EvalEnv Map.empty evalMachinePrimitive) chart tm'
             f :: Fix ArithI -> Term Int
             f = review termP3
         in second f $ fst resultTm

-- This one is certainly wrong
patP' :: Prism' (Pattern Void) (Fix (PatVarF :+: Arith Text))
patP' = error "TODO"

termP1 :: Prism' (Term a) (Fix (VarBindingF :+: Arith a))
termP1 = prism' rtl ltr where
  rtl = \case
    Fix (InL tm') -> review (varBindingP termP1) tm'
    Fix (InR tm') -> review (mkTermP termP1) tm'
  ltr tm' = asum
    [ Fix . InL <$> preview (varBindingP termP1) tm'
    , Fix . InR <$> preview (mkTermP termP1) tm'
    ]

termP2 :: Prism'
  (Term (Either Text E))
  (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Arith Text))
termP2 = termAdaptor _Left . machineTermP (_Fix . _PrimValue)

termP2' :: Prism'
  (Term (Either Text Void))
  (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Arith Text))
termP2' = termAdaptor _Left . machineTermP (_Fix . _PrimValue)

termP3 :: Prism' (Term a) (Fix (Arith a))
termP3 = prism' rtl ltr where
  rtl (Fix tm') = review (mkTermP termP3) tm'
  ltr tm' = Fix <$> preview (mkTermP termP3) tm'

type ArithV = Arith Void

peanoEval :: Term Void -> Either String (Term Void)
peanoEval tm =
  let tm' :: Fix (VarBindingF :+: ArithV)
      Just tm' = preview termP1 tm

  in case unMkDenotationChart patP' termP2' (forceRight peanoDynamics) of
       Nothing -> Left "couldn't unmake denotation chart"
       Just chart ->
         let resultTm :: (Either String (Fix ArithV), Seq Text)
             resultTm = eval (EvalEnv Map.empty (const Nothing)) chart tm'
             f = review termP3

         in second f $ fst resultTm

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Int" , E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Prim", E . Right <$> stringLiteral)
  ]

arithTests :: Test ()
arithTests = tests
  -- XXX
  -- [ scope "eval" $ expectEq (machineEval example) (Right (PrimInt 4))
  [ scope "prop_parse_pretty" $
    testProperty $ prop_parse_pretty syntax "Arith"
      (const Nothing) primParsers
  , scope "prop_serialise_identity" $ testProperty $
    prop_serialise_identity @() syntax "Arith" (const Nothing)
  , scope "parse" $ parseTest
      (ParseEnv syntax "Arith" UntaggedExternals primParsers)
      "Za"
      (Fix (Var "Za"))
  ]
