{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
module Linguist.Languages.SimpleExample
  ( syntax
  -- , dynamics
  , statics
  , typingJudgement
  , E
  , tm1
  , tm2
  , tm3
  , dynamicTests
  , minusTests
  , completePatternTests
  , prettySyntaxChartTests
  , prettyStaticTests
  , matchesTests
  , evalTests
  , translateTests
  , parseTests
  , propTests

  , eval'
  ) where

import           Codec.Serialise
import           Control.Lens                          hiding (from, to, op)
import           Control.Monad.Reader
import qualified Data.Map.Strict                       as Map
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest
import           GHC.Generics                          (Generic)
import           NeatInterpolation
import           Text.Megaparsec
import           Data.Sequence                         (Seq)
import           Data.Diverse.Lens.Which
import           Hedgehog                              (Property, property, (===), forAll, Gen)
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range

import qualified Linguist.ParseDenotationChart         as PD
import           Linguist.ParseLanguage
import qualified Linguist.Proceed                      as P1
import qualified Linguist.Proceed2                     as P2
import           Linguist.Types hiding (patP)
import qualified Linguist.Types as Types
import           Linguist.Languages.MachineModel
import           Linguist.FunctorUtil
import           Linguist.ParseUtil
import           Linguist.Util                         (forceRight)


newtype E = E (Either Int Text)
  deriving (Eq, Show, Generic)

instance Serialise E

makeWrapped ''E

instance AsFacet Text E where
  facet = _Wrapped . _Right

instance AsFacet Int E where
  facet = _Wrapped . _Left

instance Pretty E where
  pretty (E (Left  x)) = pretty x
  pretty (E (Right x)) = pretty ("\"" <> x <> "\"")

pattern PrimValue' :: Text -> Either Int Text -> Term E
pattern PrimValue' name x = Term name [ PrimValue (E x) ]

pattern TI :: Int -> Term E
pattern TI x = PrimValue' "Num" (Left x)

pattern TS :: Text -> Term E
pattern TS x = PrimValue' "Str" (Right x)

pattern VI :: Int -> Term E
pattern VI x = PrimValue' "NumV" (Left x)

pattern VS :: Text -> Term E
pattern VS x = PrimValue' "StrV" (Right x)

pattern VI' :: a -> Term (Either Text a)
pattern VI' x = Term "NumV" [ PrimValue (Right x) ]

pattern VS' :: a -> Term (Either Text a)
pattern VS' x = Term "StrV" [ PrimValue (Right x) ]

-- Chart of the language @e@. We use this for testing.
syntax :: SyntaxChart
syntax = SyntaxChart $ Map.fromList
  [ ("Typ", SortDef []
    [ Operator "Num" (Arity []) "numbers"
    , Operator "Str" (Arity []) "strings"
    ])
  , ("Exp", SortDef []
    [ Operator "Num" (ExternalArity "Num") "literal number"
    , Operator "Str" (ExternalArity "Str") "literal string"
    , Operator "Plus"
      (Arity ["Exp", "Exp"]) "addition"
    , Operator "Times"
      (Arity ["Exp", "Exp"]) "multiplication"
    , Operator "Cat"
      (Arity ["Exp", "Exp"]) "concatenation"
    , Operator "Len"
      (Arity ["Exp"]) "length"
    -- TODO:
    --   Check for anything matching this that it has a binding pattern in the
    --   second slot
    , Operator "Let"   (Arity ["Exp", Valence ["Exp"] "Exp"]) "definition"
    , Operator "Annotation" (Arity ["Exp", "Exp"]) "annotation"
    ])
  , ("List", SortDef ["a"]
    [ Operator "Nil" (Arity []) ""
    , Operator "Cons" (Arity ["a", Valence [] (SortAp "List" ["a"])]) ""
    ])
  ]

tm1F, tm2F, tm3F :: Fix (TermF :+: ExpF () E)
tm1F = (Fix (InR (NumLit (E (Left 2)))))
-- Fix $ InR $ Annotation () $
--   Fix $ InR $ Let
--     (Fix (InR (NumLit 1)))
--     (Fix (InL (BindingF ["x"] $ Fix $ InR $ Plus
--       (Fix (InL (VarF "x")))
--       (Fix (InR (NumLit 2)))
--       )))

tm2F = Fix $ InR $ Cat
 (Fix (InR (StrLit "foo")))
 (Fix (InR (StrLit "bar")))

tm3F = Fix $ InR $ Times
  (Fix $ InR $ Len $ Fix $ InR $ StrLit "hippo")
  (Fix $ InR $ NumLit 3)

tm1, tm2, tm3 :: Term E
tm1 = Term "Annotation"
  [ TS "annotation"
  , Term "Let"
    [ TI 1
    , Binding ["x"]
      (Term "Plus"
        [ Var "x"
        , TI 2
        ])
    ]
  ]

tm2 = Term "Cat"
  [ TS "foo"
  , TS "bar"
  ]

tm3 = Term "Times"
  [ Term "Len"
    [ TS "hippo"
    ]
  , TI 3
  ]

evalMachinePrimitive :: E -> Maybe (Seq (Term E) -> Term E)
evalMachinePrimitive (E (Right str)) = case str of
  "add" -> Just $ \case
    VI x :< VI y :< Empty -> VI (x + y)
    args                  -> error $ "bad call to add: " ++ show args
  "mul" -> Just $ \case
    VI x :< VI y :< Empty -> VI (x * y)
    _                     -> error "bad call to mul"
  "cat" -> Just $ \case
    VS x :< VS y :< Empty -> VS (x <> y)
    _                     -> error "bad call to cat"
  "len" -> Just $ \case
    VS x :< Empty -> VI (Text.length x)
    _             -> error "bad call to len"
  _ -> Nothing
evalMachinePrimitive _ = Nothing

data ExpF t p e
  = Plus       !e !e
  | Times      !e !e
  | Cat        !e !e
  | Len        !e
  | Let        !e !e
  | Annotation !t !e
  | NumLit     !p
  | StrLit     !p
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Eq p => Zippable (ExpF () p) where
  fzip f (Plus       a1 b1) (Plus       a2 b2) = Plus  <$> f a1 a2 <*> f b1 b2
  fzip f (Times      a1 b1) (Times      a2 b2) = Times <$> f a1 a2 <*> f b1 b2
  fzip f (Cat        a1 b1) (Cat        a2 b2) = Cat   <$> f a1 a2 <*> f b1 b2
  fzip f (Len        a1   ) (Len        a2   ) = Len   <$> f a1 a2
  fzip f (Let        a1 b1) (Let        a2 b2) = Let   <$> f a1 a2 <*> f b1 b2
  fzip f (Annotation () b1) (Annotation () b2) = Annotation ()     <$> f b1 b2
  fzip _ (NumLit a1   )     (NumLit a2   )
    = NumLit <$> if a1 == a2 then Just a1 else Nothing
  fzip _ (StrLit a1   )     (StrLit a2   )
    = StrLit <$> if a1 == a2 then Just a1 else Nothing
  fzip _ _                  _
    = Nothing

instance Show t => Show2 (ExpF t) where
  liftShowsPrec2 showsa _ showse _ p expf = showParen (p > 10) $ case expf of
    Plus       a b -> ss "Plus "       . showse    11 a . ss " " . showse 11 b
    Times      a b -> ss "Times "      . showse    11 a . ss " " . showse 11 b
    Cat        a b -> ss "Cat "        . showse    11 a . ss " " . showse 11 b
    Len        a   -> ss "Len "        . showse    11 a
    Let        a b -> ss "Let "        . showse    11 a . ss " " . showse 11 b
    Annotation t e -> ss "Annotation " . showsPrec 11 t . ss " " . showse 11 e
    NumLit     i   -> ss "NumLit "     . showsa    11 i
    StrLit     s   -> ss "StrLit "     . showsa    11 s
    where ss = showString

instance Eq t => Eq2 (ExpF t) where
  liftEq2 _   eqe (Plus       a1 b1) (Plus       a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Times      a1 b1) (Times      a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Cat        a1 b1) (Cat        a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Len        a1   ) (Len        a2   ) = eqe a1 a2
  liftEq2 _   eqe (Let        a1 b1) (Let        a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Annotation t1 e1) (Annotation t2 e2) = t1 == t2  && eqe e1 e2
  liftEq2 eqa  _  (NumLit     i1   ) (NumLit     i2   ) = eqa i1 i2
  liftEq2 eqa  _  (StrLit     s1   ) (StrLit     s2   ) = eqa s1 s2
  liftEq2 _   _   _                  _                  = False

data ValF prim val
  = NumV !prim
  | StrV !prim
  | PrimAdd !val !val
  | PrimMul !val !val
  | PrimCat !val !val
  | PrimLen !val
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor ValF where
  bimap = undefined

instance Show2 ValF where
  liftShowsPrec2 showa _ _ _ p (NumV i) = showsPrec p i
  liftShowsPrec2 showa _ _ _ p (StrV s) = showsPrec p s

  liftShowsPrec2 _ _ showse _ p valf = showParen (p > 10) $ case valf of
    PrimAdd a b -> ss "PrimAdd " . showse 11 a . ss " " . showse 11 b
    PrimMul a b -> ss "PrimMul " . showse 11 a . ss " " . showse 11 b
    PrimCat a b -> ss "PrimCat " . showse 11 a . ss " " . showse 11 b
    PrimLen a   -> ss "PrimLen " . showse 11 a
    _           -> error "vacuous match"
    where ss = showString

instance Eq2 ValF where
  liftEq2 eqa _   (NumV i1)       (NumV i2)       = eqa i1 i2
  liftEq2 eqa _   (StrV s1)       (StrV s2)       = eqa s1 s2
  liftEq2 _   eqe (PrimAdd a1 b1) (PrimAdd a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (PrimMul a1 b1) (PrimMul a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (PrimCat a1 b1) (PrimCat a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (PrimLen a1   ) (PrimLen a2   ) = eqe a1 a2
  liftEq2 _   _   _               _               = False

-- TODO: non-erased types?
dynamicsF :: DenotationChart' (ExpF () Text) (MachineF :+: ValF Text)
dynamicsF = DenotationChart'
  [ matchop Plus  :-> rhsop PrimAdd
  , matchop Times :-> rhsop PrimMul
  , matchop Cat   :-> rhsop PrimCat
  , Fix (InR (Len (Fix (InL (PatVarF (Just "e"))))))
    :->
    Fix (InR (InR (InR (PrimLen (Fix (InR (InL (MeaningOf "e"))))))))
  , Fix (InR (Let
      (Fix (InL (PatVarF (Just "e"))))
      (Fix (InL (PatVarF (Just "body"))))))
    :->
    Fix (InR (InR (InL (App
      (Fix $ InR $ InL $ MeaningOf "body")
      (Fix $ InR $ InL $ MeaningOf "e")))))
  , Fix (InR (Annotation () (Fix (InL (PatVarF (Just "contents"))))))
    :->
    Fix (InR (InL (MeaningOf "contents")))

  , Fix (InR (NumLit "i"))
    :->
    Fix (InR (InR (InR (NumV "i"))))

  ] where

  matchop
    :: Functor f
    => ( Fix (PatF :+: f)
      -> Fix (PatF :+: f)
      -> ExpF () Text (Fix (PatF :+: ExpF () Text)))
    -> Fix (PatF :+: ExpF () Text)
  matchop op = FixIn (op
    (Fix (InL (PatVarF (Just "n1"))))
    (Fix (InL (PatVarF (Just "n2")))))

  rhsop
    :: (f ~ (TermF :+: MeaningOfF :+: MachineF :+: ValF Text))
    => (Fix f -> Fix f -> ValF Text (Fix f)) -> Fix f
  rhsop op = Fix (InR (InR (InR (op
    (Fix (InR (InL (MeaningOf "n1"))))
    (Fix (InR (InL (MeaningOf "n2"))))))))

dynamicsT :: Text
dynamicsT = [text|
  [[ Plus(n1; n2)  ]]       = PrimApp({add}; [[ n1 ]]; [[ n2 ]])
  [[ Times(n1; n2) ]]       = PrimApp({mul}; [[ n1 ]]; [[ n2 ]])
  [[ Cat(n1; n2)   ]]       = PrimApp({cat}; [[ n1 ]]; [[ n2 ]])
  [[ Len(e) ]]              = PrimApp({len}; [[ e ]]))
  [[ Let(e; body) ]]        = App([[ body ]]; [[ e ]])
  [[ Annotation(_; body) ]] = [[ body ]]
  [[ Num(i) ]]              = NumV([[ i ]])
  [[ Str(s) ]]              = StrV([[ s ]])
  |]

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left        <$> intLiteral
  , Right "add" <$  symbol "add"
  , Right "mul" <$  symbol "mul"
  , Right "cat" <$  symbol "cat"
  , Right "len" <$  symbol "len"
  ]

dynamics'
  :: Either (ParseErrorBundle Text Void) (DenotationChart E (Either Text E))
dynamics' = runParser (PD.parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" dynamicsT

dynamics :: DenotationChart Text (Either Text Text)
dynamics = mkDenotationChart patP valP dynamicsF

patP :: forall a. Prism' (Pattern a) (Fix (PatF :+: ExpF () a))
patP = prism' rtl ltr where
  rtl = \case
    Fix (InL pat) -> review (Types.patP patP) pat
    Fix (InR pat) -> review patP'             pat
  ltr tm = msum
    [ Fix . InL <$> preview (Types.patP patP) tm
    , Fix . InR <$> preview patP'             tm
    ]

  p :: Prism' (Pattern a) a
  p = _PatternPrimVal . _Just

--   iP :: Prism' (Pattern E) Int
--   iP = _PatternPrimVal . _Just . facet

--   sP :: Prism' (Pattern E) Text
--   sP = _PatternPrimVal . _Just . facet

  patP' :: Prism' (Pattern a) (ExpF () a (Fix (PatF :+: ExpF () a)))
  patP' = prism' rtl' ltr' where
    rtl' = \case
      Plus a b  -> PatternTm "Plus"  [ review patP a , review patP b ]
      Times a b -> PatternTm "Times" [ review patP a , review patP b ]
      Cat a b   -> PatternTm "Cat"   [ review patP a , review patP b ]
      Len a     -> PatternTm "Len"   [ review patP a                 ]
      Let a b   -> PatternTm "Let"   [ review patP a , review patP b ]
      Annotation () a -> PatternTm "Annotation" [ review patP a ]
      NumLit i  -> PatternTm "NumLit" [ review p i ]
      StrLit s  -> PatternTm "StrLit" [ review p s ]
    ltr' = \case
      PatternTm "Plus"       [ a, b ] -> Plus  <$> preview patP a <*> preview patP b
      PatternTm "Times"      [ a, b ] -> Times <$> preview patP a <*> preview patP b
      PatternTm "Cat"        [ a, b ] -> Cat   <$> preview patP a <*> preview patP b
      PatternTm "Len"        [ a    ] -> Len   <$> preview patP a
      PatternTm "Let"        [ a, b ] -> Let   <$> preview patP a <*> preview patP b
      PatternTm "Annotation" [ a    ] -> Annotation () <$> preview patP a
      PatternTm "NumLit"     [ i    ] -> NumLit <$> preview p i
      PatternTm "StrLit"     [ s    ] -> StrLit <$> preview p s
      _                               -> Nothing

valP :: forall a. Prism'
  (Term (Either Text a))
  (Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF a))
valP = prism' rtl ltr where
  rtl :: Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF a) -> Term (Either Text a)
  rtl = \case
    Fix (InL tm')             -> review (termP    valP) tm'
    Fix (InR (InL tm'))       -> review meaningP        tm'
    Fix (InR (InR (InL tm'))) -> review (machineP valP) tm'
    Fix (InR (InR (InR tm'))) -> review valP'           tm'

  ltr :: Term (Either Text a) -> Maybe (Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF a))
  ltr tm = msum
    [ Fix . InL             <$> preview (termP    valP) tm
    , Fix . InR . InL       <$> preview meaningP        tm
    , Fix . InR . InR . InL <$> preview (machineP valP)           tm
    , Fix . InR . InR . InR <$> preview valP'           tm
    ]

  valP' :: Prism'
    (Term (Either Text a))
    (ValF a (Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF a)))
  valP' = prism' rtl' ltr' where
    rtl' = \case
      NumV i      -> VI' i
      StrV s      -> VS' s
      PrimLen a   -> Term "PrimLen" [ review valP a                ]
      PrimAdd a b -> Term "PrimAdd" [ review valP a, review valP b ]
      PrimMul a b -> Term "PrimMul" [ review valP a, review valP b ]
      PrimCat a b -> Term "PrimCat" [ review valP a, review valP b ]
    ltr' = \case
      VI' i                   -> Just $ NumV i
      VS' s                   -> Just $ StrV s
      Term "PrimLen" [ a    ] -> PrimLen <$> preview valP a
      Term "PrimAdd" [ a, b ] -> PrimAdd <$> preview valP a <*> preview valP b
      Term "PrimMul" [ a, b ] -> PrimMul <$> preview valP a <*> preview valP b
      Term "PrimCat" [ a, b ] -> PrimCat <$> preview valP a <*> preview valP b
      _                       -> Nothing

genText :: Gen Text
genText = Gen.text (Range.exponential 0 1000) Gen.unicode

pattern FixExp
  :: ExpF () E (Fix (PatF :+: ExpF () E))
  -> Fix            (PatF :+: ExpF () E)
pattern FixExp x = Fix (InR x)

genPat :: Gen (Fix (PatF :+: ExpF () E))
genPat = Gen.recursive Gen.choice [
    Fix . InL . PatVarF <$> Gen.choice
      [ pure Nothing
      , Just <$> genText
      ]
  ] [
    Gen.subtermM genPat $ \x -> fmap (Fix . InL) $ PatBindingF
      <$> Gen.list (Range.exponential 0 15) genText
      <*> pure x
  , Gen.subterm2 genPat genPat (fmap FixExp . Plus)
  , Gen.subterm2 genPat genPat (fmap FixExp . Times)
  , Gen.subterm2 genPat genPat (fmap FixExp . Cat)
  , Gen.subterm  genPat        (     FixExp . Len)
  , Gen.subterm2 genPat genPat (fmap FixExp . Let)
  , Gen.subterm  genPat        (     FixExp . Annotation ())
  ]

pattern FixVal
  :: ValF E (Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF E))
  -> Fix         (TermF :+: MeaningOfF :+: MachineF :+: ValF E)
pattern FixVal x = Fix (InR (InR (InR x)))

-- TODO: generate TermF / MachineF as well
genVal :: Gen (Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF E))
genVal = Gen.recursive Gen.choice [
    FixVal . NumV . E . Left  <$> Gen.int Range.exponentialBounded
  , FixVal . StrV . E . Right <$> genText
  ] [
    Gen.subterm2 genVal genVal (fmap FixVal . PrimAdd)
  , Gen.subterm2 genVal genVal (fmap FixVal . PrimMul)
  , Gen.subterm2 genVal genVal (fmap FixVal . PrimCat)
  , Gen.subterm  genVal        (     FixVal . PrimLen)
  ]

patP_round_trip_prop :: Property
patP_round_trip_prop = property $ do
  x <- forAll genPat
  preview patP (review patP x) === Just x

valP_round_trip_prop :: Property
valP_round_trip_prop = property $ do
  x <- forAll genVal
  preview valP (review valP x) === Just x

dynamicTests :: Test ()
dynamicTests =
  let
      n1, n2, lenStr :: Term E
      lenStr      = Term "Len"   [ TS "str" ]
      times v1 v2 = Term "Times" [ v1, v2   ]
      plus v1 v2  = Term "Plus"  [ v1, v2   ]
      n1          = TI 1
      n2          = TI 2
      x           = PatternVar (Just "x")
      patCheck    :: Maybe (PatternCheckResult Text)
      patCheck    = runMatches syntax "Exp" $ patternCheck dynamics
      env         = MatchesEnv syntax "Exp" $ Map.singleton "x" $ VI 2
  in tests
       [ expectJust $ runMatches syntax "Exp" $ matches x
         lenStr
       , expectJust $ runMatches syntax "Exp" $ matches (PatternTm "Len" [x])
         lenStr
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         lenStr
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         (times n1 n2)
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         (plus n1 n2)
       , expectJust $ runMatches syntax "Exp" $ findMatch (forceRight dynamics')
         tm1

--        , let Just (subst, _) = findMatch syntax "Exp" dynamics tm1
--              result = applySubst subst tm1
--        , expect $ result == tm1 & ix 0 .~

       , expectJust $
         let pat = PatternTm "Cat"
               [ PatternVar (Just "s_1")
               , PatternVar (Just "s_2")
               ]
         in runMatches syntax "Exp" $ matches pat tm2
       , expectJust $
         let pat :: Pattern E
             pat = PatternVar (Just "n_1")
             tm  = Var "x"
         in flip runReaderT env $ matches pat tm
       , expectJust $
         let pat = PatternVar (Just "n_2")
             tm  = TI 2
         in flip runReaderT env $ matches pat tm
       , expectJust $
         let pat = PatternTm "Plus"
               [ PatternVar (Just "n_1")
               , PatternVar (Just "n_2")
               ]
             tm = Term "Plus" [Var "x", TI 2]
         in flip runReaderT env $ matches pat tm

       , expect $
         fmap isComplete patCheck
         ==
         Just False

--        , expect $ fmap hasRedundantPat patCheck
--          ==
--          Just True

       ]

typingJudgement :: JudgementForm
typingJudgement = JudgementForm "types" [(JIn, "Exp"), (JIn, "Typ")]

statics :: JudgementRules
statics =
  let tm -: ty = [tm, ty] %%% "types"
  in JudgementRules
  [ ["x" -: "num", "y" -: "num"]
    .--
    ("plus" @@ ["x", "y"]) -: "num"
  , ["x" -: "num", "y" -: "num"]
    .--
    ("times" @@ ["x", "y"]) -: "num"
  , ["x" -: "str", "y" -: "str"]
    .--
    ("cat" @@ ["x", "y"]) -: "str"
  , ["x" -: "str"]
    .--
    ("len" @@ ["x"]) -: "num"
  -- TODO: how to do assumptions?
  -- , ["x" -: "a"]
  --   .--
  --   ("let" @@ ["x", _]) -: "a"
  ]

expectEq' :: Maybe (Pattern Void) -> Maybe (Pattern Void) -> Test ()
expectEq' = expectEq

completePatternTests :: Test ()
completePatternTests = scope "completePattern" $ tests
  [ expectEq'
      (runMatches syntax "Typ" completePattern)
      (Just (PatternUnion [PatternTm "Num" [], PatternTm "Str" []]))
  , expectEq'
      (runMatches syntax "Exp" completePattern)
      (Just (PatternUnion
        [ PatternTm "Num"        [ PatternPrimVal Nothing ]
        , PatternTm "Str"        [ PatternPrimVal Nothing ]
        , PatternTm "Plus"       [ PatternAny, PatternAny ]
        , PatternTm "Times"      [ PatternAny, PatternAny ]
        , PatternTm "Cat"        [ PatternAny, PatternAny ]
        , PatternTm "Len"        [ PatternAny             ]
        , PatternTm "Let"        [ PatternAny, PatternAny ]
        , PatternTm "Annotation" [ PatternAny, PatternAny ]
        ]))
  ]

minusTests :: Test ()
minusTests = scope "minus" $
  let x = PatternVar (Just "x")
      y = PatternVar (Just "y")
      num = PatternTm "num" [ PatternPrimVal Nothing ]
      any' = PatternAny
  in tests
       [ expectEq' (runMatches undefined undefined (minus x x)) (Just PatternEmpty)
       , expectEq' (runMatches undefined undefined (minus x y)) (Just PatternEmpty)
       , expectEq' (runMatches undefined undefined (minus x any')) (Just PatternEmpty)
       -- , expectEq' (runMatches undefined undefined (minus any' x)) (Just PatternEmpty)
       , expectEq' (runMatches undefined undefined (minus any' any')) (Just PatternEmpty)
       , expectEq'
         (runMatches syntax "Typ" (minus (PatternTm "num" []) (PatternTm "num" [])))
         (Just PatternEmpty)
       , expectEq' (runMatches syntax "Exp" (minus num num)) (Just PatternEmpty)
       , expectEq'
         (runMatches syntax "Exp" (minus
           (PatternTm "plus"
             [ PatternTm "num" [ PatternPrimVal Nothing ]
             , x
             ])
           (PatternTm "plus"
             [ PatternTm "num" [ PatternPrimVal Nothing ]
             , y
             ])))
         (Just PatternEmpty)


       -- This is wrong:
       -- , expect $
       --   let env = MatchesEnv syntax "Exp" $ Map.fromList
       --         -- TODO: we should be able to do this without providing values
       --         [ ("x", TI 2)
       --         , ("y", TI 2)
       --         ]
       --   in (traceShowId $ flip runReaderT env (minus
       --        (PatternTm "plus" [x, y])
       --        (PatternTm "plus"
       --          [ PatternPrimVal "num" Nothing
       --          , PatternPrimVal "num" Nothing
       --          ])))
       --        ==
       --        Just PatternEmpty
       ]

prettySyntaxChartTests :: Test ()
prettySyntaxChartTests = tests
  [ expectEq
    (renderStrict (layoutPretty defaultLayoutOptions (pretty syntax)))
    (Text.init [text|
      Exp ::=
        Num{Num}
        Str{Str}
        Plus(Exp; Exp)
        Times(Exp; Exp)
        Cat(Exp; Exp)
        Len(Exp)
        Let(Exp; Exp.Exp)
        Annotation(Exp; Exp)
      List a ::=
        Nil
        Cons(a; List a)
      Typ ::=
        Num
        Str
    |])
  ]

prettyStaticTests :: Test ()
prettyStaticTests = tests
  [ expect $
    renderStrict (layoutPretty defaultLayoutOptions (pretty statics))
    ==
    Text.intercalate "\n"
    [ "types x num, types y num"
    , "------"
    , "types (plus x y) num"
    , ""
    , "types x num, types y num"
    , "------"
    , "types (times x y) num"
    , ""
    , "types x str, types y str"
    , "------"
    , "types (cat x y) str"
    , ""
    , "types x str"
    , "------"
    , "types (len x) num"
    ]
  ]

matchesTests :: Test ()
matchesTests = scope "matches" $
  let foo :: Term ()
      foo = Term "foo" []

  -- We intentionally include these `undefined`s to check that we don't depend
  -- on the syntax / sort.
  in tests
       [ expectJust $ runMatches undefined undefined $ matches
         (PatternVar (Just "x")) foo
       , expectJust $ runMatches undefined undefined $ matches
         PatternAny foo
       , expectJust $ runMatches undefined undefined $ matches
         (PatternUnion [PatternAny, undefined]) foo
       , expect $
         (runMatches syntax "Typ" $ matches
           (BindingPattern ["y"] PatternAny)
           (Binding ["x"] (Term "num" [])))
         ==
         (Just (Subst Map.empty [("y", "x")])
           :: Maybe (Subst ()))
       ]

eval' :: Term E -> Either String (Term E)
eval' = P1.eval $ P1.mkEvalEnv "Exp" syntax (forceRight dynamics')
  evalMachinePrimitive
  Just

evalF :: Fix (TermF :+: ExpF () E) -> Either String (Fix (ValF E))
evalF = P2.eval (P2.EvalEnv Map.empty evalMachinePrimitiveF) dynamicsF

evalMachinePrimitiveF :: Text -> Maybe (Seq (ValF E (Fix (ValF E))) -> ValF E (Fix (ValF E)))
evalMachinePrimitiveF = \case
  "add" -> Just $ \case
    NumV x :< NumV y :< Empty -> NumV (x + y)
    args                      -> error $ "bad call to add: " ++ show args
  "mul" -> Just $ \case
    NumV x :< NumV y :< Empty -> NumV (x * y)
    args                      -> error $ "bad call to mul: " ++ show args
  "cat" -> Just $ \case
    StrV x :< StrV y :< Empty -> StrV (x <> y)
    args                      -> error $ "bad call to cat: " ++ show args
  "len" -> Just $ \case
    StrV x           :< Empty -> NumV (Text.length x)
    args                      -> error $ "bad call to len: " ++ show args

  -- TODO
  -- PrimAdd (NumV x) (NumV y) -> NumV (x + y)
  -- PrimMul (NumV x) (NumV y) -> NumV (x * y)
  -- PrimCat (StrV x) (StrV y) -> StrV (x <> y)
  -- PrimLen (StrV x)          -> NumV (Text.length x)
  _ -> Nothing

evalTests :: Test ()
evalTests = tests
  -- [ expectEq (eval' tm1) (Right (VI 3))
  -- , expectEq (eval' tm2) (Right (VS "foobar"))
  -- , expectEq (eval' tm3) (Right (VI 15))

  [ expectEq (evalF tm1F) (Right (Fix (NumV 3)))
  , expectEq (evalF tm2F) (Right (Fix (StrV "foobar")))
  , expectEq (evalF tm3F) (Right (Fix (NumV 15)))
  ]

translateTests :: Test ()
translateTests = tests
  [ expectEq (P2.translate dynamicsF tm1F) $ Just $
    -- (Fix (TermF :+: MeaningOfF :+: MachineF :+: ValF E))
    Fix $ InR $ InL $ App
      (Fix $ InL $ BindingF ["x"] $
        Fix $ InR $ InR $ PrimAdd
          (Fix $ InL $ VarF "x")
          (Fix $ InR $ InR $ NumV 2))
      (Fix $ InR $ InR $ NumV 1)
  ]

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Num", E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Str", E . Right <$> stringLiteral)
  ]

parseTests :: Test ()
parseTests =
  let runP sty str = runParser
        (runReaderT standardParser (ParseEnv syntax "Exp" sty primParsers))
        "(test)" str
      expectParse sty str tm = scope (Text.unpack str) $ case runP sty str of
        Left err       -> fail $ errorBundlePretty err
        Right parsedTm -> expectEq parsedTm tm
      expectNoParse sty str = scope (Text.unpack str) $ case runP sty str of
        Left _   -> ok
        Right tm -> fail $ "parsed " ++ show tm
  in scope "parse" $ tests
  [ expectParse UntaggedExternals
      "Plus(1; 2)"
      (Term "Plus" [TI 1, TI 2])
  , expectParse UntaggedExternals
      "Cat(\"abc\"; \"def\")"
      (Term "Cat" [TS "abc", TS "def"])
  , expectParse UntaggedExternals
      "\"\\\"quoted text\\\"\""
      (TS "\\\"quoted text\\\"")

  , expectNoParse UntaggedExternals "\"ab\\\""
  , expectNoParse TaggedExternals "\"ab\\\""

  -- Note this doesn't check but it should still parse
  , expectParse UntaggedExternals
      "Cat(\"abc\"; 1)"
      (Term "Cat" [TS "abc", TI 1])
  , expectNoParse UntaggedExternals
      "Cat(Str{\"abc\"}; Num{1})"
  , expectParse TaggedExternals
      "Cat(Str{\"abc\"}; Num{1})"
      (Term "Cat" [TS "abc", TI 1])
  , expectNoParse TaggedExternals
      "Cat(\"abc\"; 1)"

  , expectParse UntaggedExternals
     "Let(1; x. Plus(x; 2))"
     (Term "Let"
       [ TI 1
       , Binding ["x"]
         (Term "Plus"
           [ Var "x"
           , TI 2
           ])
       ])

  , expectParse UntaggedExternals "0" (TI 0)
  ]

propTests :: Test ()
propTests =
  let aGen = \case
        "Num" -> Just $ E . Left  <$> Gen.int Range.exponentialBounded
        "Str" -> Just $ E . Right <$> genText
        _     -> Nothing
  in tests
       [ scope "prop_parse_pretty" $ testProperty $
         prop_parse_pretty syntax "Exp" aGen primParsers

       , scope "prop_serialise_identity" $ testProperty $
         prop_serialise_identity syntax "Exp" aGen

       , scope "patP_round_trip_prop" $ testProperty $
         patP_round_trip_prop

       , scope "valP_round_trip_prop" $ testProperty $
         valP_round_trip_prop
       ]
