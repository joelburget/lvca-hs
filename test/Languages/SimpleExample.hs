{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Languages.SimpleExample
  ( dynamicTests
  , minusTests
  , completePatternTests
  , prettySyntaxChartTests
  , prettyStaticTests
  , matchesTests
  , evalTests
  , translateTests
  , parseTests
  , propTests
  , explicitPatP

  -- currently unused:
  , tm3
  , typingJudgement
  ) where

import           Codec.Serialise
import           Control.Lens                          hiding (from, to, op)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.CPS
import           Data.Sequence                         (Seq)
import           Data.String                           (IsString(fromString))
import           Data.Text                             (Text)
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest
import           GHC.Generics                          (Generic)
import           Hedgehog                              (Property, property, (===), forAll, Gen)
import           NeatInterpolation
import           Text.Megaparsec
import qualified Data.Map.Strict                       as Map
import qualified Data.Text                             as Text
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range

import Lvca
import Lvca.Types (matches)

import Test.ParseLanguage
import Test.Types

newtype E = E (Either Int Text)
  deriving (Eq, Show, Generic)

instance Serialise E

makeWrapped ''E

instance Pretty E where
  pretty (E (Left  x)) = pretty x
  pretty (E (Right x)) = pretty ("\"" <> x <> "\"")

instance IsString E where
  fromString = E . Right . fromString

pattern PrimValue' :: Text -> Either Int Text -> Term E
pattern PrimValue' name x = Fix (Term name [ Fix (PrimValue (E x)) ])

pattern TI :: Int -> Term E
pattern TI x = PrimValue' "Num" (Left x)

pattern TS :: Text -> Term E
pattern TS x = PrimValue' "Str" (Right x)

mkTypes (Options Nothing $ Map.fromList
  [ "Annot" :-> [t| ()   |]
  , "Int"   :-> [t| Int  |]
  , "Text"  :-> [t| Text |]
  ])
  "Exp ::=                                                                  \n\
  \  Plus(Exp; Exp)                                                         \n\
  \  Times(Exp; Exp)                                                        \n\
  \  Cat(Exp; Exp)                                                          \n\
  \  Len(Exp)                                                               \n\
  \  Let(Exp; Exp)                                                          \n\
  \  Annotation({Annot}; Exp)                                               \n\
  \  NumLit{Int}                                                            \n\
  \  StrLit{Text}"
mkSyntaxInstances ''Exp

concreteSyntaxChart = [text|
  Exp :=
    // TODO: precendence
    // TODO: how to specify spaces between tokens?
    a "*" b        <=> Times(a; b)
    a "+" b        <=> Plus(a; b)
    a "++" b       <=> Cat(a; b)
    "len" a        <=> Len(a)
    "let" a "in" b <=> Let(a; b)
    a "//" b       <=> Annotation(b; a)

    // TODO: how to handle these
    i              <=> NumLit(i)
    str            <=> StrLit(str)

    "(" a ")"      <=> a
  |]

mkTypes (Options Nothing $ Map.fromList
  [ "Int"  :-> [t| Int  |]
  , "Text" :-> [t| Text |]
  ])
  "Val ::=                                                                  \n\
  \  NumV{Int}                                                              \n\
  \  StrV{Text}"
mkSyntaxInstances ''Val

dynamicsT :: Text
dynamicsT = [text|
  [[ Plus(n1; n2)        ]] = PrimApp({add}; [[ n1 ]]; [[ n2 ]])
  [[ Times(n1; n2)       ]] = PrimApp({mul}; [[ n1 ]]; [[ n2 ]])
  [[ Cat(n1; n2)         ]] = PrimApp({cat}; [[ n1 ]]; [[ n2 ]])
  [[ Len(e)              ]] = PrimApp({len}; [[ e ]]))
  [[ Let(e; body)        ]] = App([[ body ]]; [[ e ]])
  [[ Annotation(_; body) ]] = [[ body ]]
  [[ Num(i)              ]] = NumV([[ i ]])
  [[ Str(s)              ]] = StrV([[ s ]])
  |]

parsePrim :: DenotationChartParser E
parsePrim = E <$> choice
  [ Left        <$> intLiteral
  , Right "add" <$  symbol "add"
  , Right "mul" <$  symbol "mul"
  , Right "cat" <$  symbol "cat"
  , Right "len" <$  symbol "len"
  ]

dynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart E (Either Text E))
dynamics = runParser (parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" dynamicsT

tm1, tm2, tm3 :: Term E
tm1 = Fix $ Term "Annotation"
  [ TS "annotation"
  , Fix $ Term "Let"
    [ TI 1
    , Fix $ Binding ["x"] $
      Fix $ Term "Plus"
        [ Fix $ Var "x"
        , TI 2
        ]
    ]
  ]

tm2 = Fix $ Term "Cat"
  [ TS "foo"
  , TS "bar"
  ]

tm3 = Fix $ Term "Times"
  [ Fix $ Term "Len"
    [ TS "hippo"
    ]
  , TI 3
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

pattern VI :: Int -> Term E
pattern VI x = PrimValue' "NumV" (Left x)

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

genText :: Gen Text
genText = Gen.text (Range.exponential 0 1000) Gen.unicode

pattern FixExp
  :: Exp E (Fix (PatVarF :+: Exp E))
  -> Fix        (PatVarF :+: Exp E)
pattern FixExp x = Fix (InR x)

genPat :: Gen (Fix (PatVarF :+: Exp E))
genPat = Gen.recursive Gen.choice [
    Fix . InL . PatVarF <$> Gen.choice
      [ pure Nothing
      , Just <$> genText
      ]
  ] [
    Gen.subterm2 genPat genPat (fmap FixExp . Plus)
  , Gen.subterm2 genPat genPat (fmap FixExp . Times)
  , Gen.subterm2 genPat genPat (fmap FixExp . Cat)
  , Gen.subterm  genPat        (     FixExp . Len)
  , Gen.subterm2 genPat genPat (fmap FixExp . Let)
  , Gen.subterm  genPat        (     FixExp . Annotation (E (Right "annotation")))
  ]

patP_round_trip_prop :: Property
patP_round_trip_prop = property $ do
  x <- forAll genPat
  preview patVarP (review patVarP x) === Just x

valP_round_trip_prop :: Property
valP_round_trip_prop = property $ do
  x <- forAll genVal
  let termP' :: Prism' (Term E) (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Val E))
      termP' = machineTermP (_Fix . _PrimValue . _Wrapped . _Right)
  preview termP' (review termP' x) === Just x

pattern FixVal
  :: Val E (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Val E))
  -> Fix        (VarBindingF :+: MeaningOfF :+: MachineF :+: Val E)
pattern FixVal x = Fix (InR (InR (InR x)))

pattern FixApp
  :: Text
  -> [ Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Val E) ]
  -> Fix   (VarBindingF :+: MeaningOfF :+: MachineF :+: Val E)
pattern FixApp name tms = Fix (InR (InR (InL (PrimApp name tms))))

-- TODO: generate VarBindingF / MachineF as well
genVal :: Gen (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Val E))
genVal = Gen.recursive Gen.choice [
    FixVal . NumV . E . Left  <$> Gen.int Range.exponentialBounded
  , FixVal . StrV . E . Right <$> genText
  ] [
    Gen.subterm2 genVal genVal (\x y -> FixApp "add" [x, y])
  , Gen.subterm2 genVal genVal (\x y -> FixApp "mul" [x, y])
  , Gen.subterm2 genVal genVal (\x y -> FixApp "cat" [x, y])
  , Gen.subterm  genVal        (\x   -> FixApp "len" [x   ])
  ]

-- matchingTermTests :: Test ()
-- matchingTermTests = tests
--   [ expectEq tm1F tm1
--   ]

dynamicTests :: Test ()
dynamicTests =
  let
      n1, n2, lenStr :: Term E
      lenStr      = Fix $ Term "Len"   [ TS "str" ]
      times v1 v2 = Fix $ Term "Times" [ v1, v2   ]
      plus v1 v2  = Fix $ Term "Plus"  [ v1, v2   ]
      n1          = TI 1
      n2          = TI 2
      x           = PatternVar (Just "x")
      patCheck    :: Maybe (PatternCheckResult Text)
      patCheck    = runMatches syntax "Exp" $ patternCheck dynamics'
      env         = MatchesEnv syntax "Exp" $ Map.singleton "x" $ VI 2
  in tests
       [ expectJust $ runMatches syntax "Exp" $ matches x
         lenStr
       , expectJust $ runMatches syntax "Exp" $ matches (PatternTm "Len" [x])
         lenStr
       -- XXX
       -- , expectJust $ runMatches syntax "Exp" $ findMatch (forceRight dynamics)
       --   lenStr
       -- , expectJust $ runMatches syntax "Exp" $ findMatch (forceRight dynamics)
       --   (times n1 n2)
       -- , expectJust $ runMatches syntax "Exp" $ findMatch (forceRight dynamics)
       --   (plus n1 n2)
       -- , expectJust $ runMatches syntax "Exp" $ findMatch (forceRight dynamics)
       --   tm1

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
             tm  = Fix $ Var "x"
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
             tm = Fix $ Term "Plus" [Fix $ Var "x", TI 2]
         in flip runReaderT env $ matches pat tm

       , expect $
         fmap isComplete patCheck
         ==
         Just False

--        , expect $ fmap hasRedundantPat patCheck
--          ==
--          Just True

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
      foo = Fix $ Term "foo" []

      -- We intentionally include these `undefined`s to check that we don't
      -- depend on the syntax / sort.
      unforced = error "this term should not be forced"

  in tests
       [ expectJust $ runMatches unforced unforced $ matches
         (PatternVar (Just "x")) foo
       , expectJust $ runMatches unforced unforced $ matches
         PatternAny foo
       , expectJust $ runMatches unforced unforced $ matches
         (PatternUnion [PatternAny, unforced]) foo
       , expect $
         (runMatches syntax "Typ" $ matches
           PatternAny
           (Fix $ Binding ["x"] $ Fix $ Term "num" []))
         ==
         (Just mempty :: Maybe (Subst ()))
       ]

evalTests :: Test ()
evalTests =
  let run' tm expected = case evalF tm of
        (Left err,     _logs) -> fail err
        (Right result, _logs) -> expectEq result expected
  in tests
       [ run' tm1F $ Fix $ NumV $ E $ Left 3
       , run' tm2F $ Fix $ StrV "foobar"
       , run' tm3F $ Fix $ NumV $ E $ Left 15
       ]

translateTests :: Test ()
translateTests =
  let run' tm expected = case runWriter (runMaybeT (translate dynamicsF tm)) of
        (Nothing,     _logs) -> fail "couldn't find match"
        (Just result, _logs) -> expectEq result expected
  in tests
       [ run' (Fix $ InR $ NumLit $ E $ Left 2) $
           Fix $ InR $ InR $ NumV $ Right $ E $ Left 2

       , run' (Fix $ InR $ StrLit $ E $ Right "str") $
           Fix $ InR $ InR $ StrV $ Right $ E $ Right "str"

       , run' tm1F $
           Fix $ InR $ InL $ App
             (Fix $ InR $ InL $ Lam $
               Fix $ InL $ BindingF ["x"] $
                 Fix $ InR $ InL $ PrimApp "add"
                   [ Fix $ InL $ VarF "x"
                   , Fix $ InR $ InR $ NumV $ Right $ E $ Left 2
                   ])
             (Fix $ InR $ InR $ NumV $ Right $ E $ Left 1)

       , run' tm2F $
           Fix $ InR $ InL $ PrimApp "cat"
             [ Fix $ InR $ InR $ StrV $ Right $ E $ Right "foo"
             , Fix $ InR $ InR $ StrV $ Right $ E $ Right "bar"
             ]

       , run' tm3F $
           Fix $ InR $ InL $ PrimApp "mul"
             [ Fix $ InR $ InL $ PrimApp "len"
               [ Fix $ InR $ InR $ StrV $ Right $ E $ Right "hippo" ]
             , Fix $ InR $ InR $ NumV $ Right $ E $ Left 3
             ]
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
      (Fix $ Term "Plus" [TI 1, TI 2])
  , expectParse UntaggedExternals
      "Cat(\"abc\"; \"def\")"
      (Fix $ Term "Cat" [TS "abc", TS "def"])
  , expectParse UntaggedExternals
      "\"\\\"quoted text\\\"\""
      (TS "\\\"quoted text\\\"")

  , expectNoParse UntaggedExternals "\"ab\\\""
  , expectNoParse TaggedExternals "\"ab\\\""

  -- Note this doesn't check but it should still parse
  , expectParse UntaggedExternals
      "Cat(\"abc\"; 1)"
      (Fix $ Term "Cat" [TS "abc", TI 1])
  , expectNoParse UntaggedExternals
      "Cat(Str{\"abc\"}; Num{1})"
  , expectParse TaggedExternals
      "Cat(Str{\"abc\"}; Num{1})"
      (Fix $ Term "Cat" [TS "abc", TI 1])
  , expectNoParse TaggedExternals
      "Cat(\"abc\"; 1)"

  , expectParse UntaggedExternals
     "Let(1; x. Plus(x; 2))"
     (Fix $ Term "Let"
       [ TI 1
       , Fix $ Binding ["x"] $
         Fix $ Term "Plus"
           [ Fix $ Var "x"
           , TI 2
           ]
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

dynamics' :: DenotationChart Text Text
dynamics' = mkDenotationChart patVarP (machineTermP (_Fix . _PrimValue)) dynamicsF

evalF
  :: Fix (VarBindingF :+: Exp E)
  -> (Either String (Fix (Val E)), Seq Text)
evalF = eval (EvalEnv Map.empty evalMachinePrimitiveF) dynamicsF

-- TODO: non-erased types?
dynamicsF :: DenotationChart' (Exp Text) (MachineF :+: Val Text)
dynamicsF = DenotationChart'
  [ matchop Plus  :-> rhsop "add"
  , matchop Times :-> rhsop "mul"
  , matchop Cat   :-> rhsop "cat"

  , Fix (InR $ Len $ Fix $ InL $ PatVarF $ Just "e")
    :->
    Fix (InR $ InR $ InL $ PrimApp "len" [ Fix $ InR $ InL $ MeaningOf "e" ])

  , Fix (InR (Let
      (Fix $ InL $ PatVarF $ Just "e")
      (Fix $ InL $ PatVarF $ Just "body")))
    :->
    Fix (InR (InR (InL (App
      (Fix $ InR $ InR $ InL $ Lam $ Fix $ InR $ InL $ MeaningOf "body")
      (Fix $ InR $ InL $ MeaningOf "e")))))

  , Fix (InR (Annotation "annotation" (Fix (InL (PatVarF (Just "contents"))))))
    :->
    Fix (InR (InL (MeaningOf "contents")))

  , Fix (InR (NumLit "i"))
    :->
    Fix (InR (InR (InR (NumV "i"))))

  , Fix (InR (StrLit "i"))
    :->
    Fix (InR (InR (InR (StrV "i"))))

  ] where

  matchop
    :: Functor f
    => ( Fix (PatVarF :+: f)
      -> Fix (PatVarF :+: f)
      -> Exp Text (Fix (PatVarF :+: Exp Text)))
    -> Fix (PatVarF :+: Exp Text)
  matchop op = FixIn $ op
    (Fix $ InL $ PatVarF $ Just "n1")
    (Fix $ InL $ PatVarF $ Just "n2")

  rhsop
    :: (f ~ Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: Val Text))
    => Text -> f
  rhsop name = Fix $ InR $ InR $ InL $ PrimApp name
    [ Fix $ InR $ InL $ MeaningOf "n1"
    , Fix $ InR $ InL $ MeaningOf "n2"
    ]

explicitPatP :: forall a. Prism' (Pattern a) (Fix (PatVarF :+: Exp a))
explicitPatP = prism' rtl ltr where
  rtl = \case
    Fix (InL pat) -> review patVarP' pat
    Fix (InR pat) -> review patP    pat
  ltr tm = msum
    [ Fix . InL <$> preview patVarP' tm
    , Fix . InR <$> preview patP    tm
    ]

  p :: Prism' (Pattern a) a
  p = _PatternPrimVal . _Just

  patP :: Prism' (Pattern a) (Exp a (Fix (PatVarF :+: Exp a)))
  patP = prism' rtl' ltr' where
    rtl' = \case
      Plus  a b -> PatternTm "Plus"  [ review explicitPatP a , review explicitPatP b ]
      Times a b -> PatternTm "Times" [ review explicitPatP a , review explicitPatP b ]
      Cat   a b -> PatternTm "Cat"   [ review explicitPatP a , review explicitPatP b ]
      Len   a   -> PatternTm "Len"   [ review explicitPatP a                 ]
      Let   a b -> PatternTm "Let"   [ review explicitPatP a , review explicitPatP b ]
      Annotation a b -> PatternTm "Annotation" [ review p a,   review explicitPatP b ]
      NumLit i  -> PatternTm "NumLit" [ review p i ]
      StrLit s  -> PatternTm "StrLit" [ review p s ]
    ltr' = \case
      PatternTm "Plus"       [ a, b ] -> Plus  <$> preview explicitPatP a <*> preview explicitPatP b
      PatternTm "Times"      [ a, b ] -> Times <$> preview explicitPatP a <*> preview explicitPatP b
      PatternTm "Cat"        [ a, b ] -> Cat   <$> preview explicitPatP a <*> preview explicitPatP b
      PatternTm "Len"        [ a    ] -> Len   <$> preview explicitPatP a
      PatternTm "Let"        [ a, b ] -> Let   <$> preview explicitPatP a <*> preview explicitPatP b
      PatternTm "Annotation" [ a, b ] -> Annotation <$> preview p a <*> preview explicitPatP b
      PatternTm "NumLit"     [ i    ] -> NumLit <$> preview p i
      PatternTm "StrLit"     [ s    ] -> StrLit <$> preview p s
      _                               -> Nothing

--   valP' = prism' rtl' ltr' where
--     rtl' = \case
--       NumV i      -> Fix $ Term "NumV" [ Fix $ PrimValue $ Right i ]
--       StrV s      -> Fix $ Term "StrV" [ Fix $ PrimValue $ Right s ]
--     ltr' (Fix tm) = case tm of
--       Term "NumV" [ Fix (PrimValue (Right i)) ] -> Just $ NumV i
--       Term "StrV" [ Fix (PrimValue (Right s)) ] -> Just $ StrV s
--       _                       -> Nothing

tm1F, tm2F, tm3F :: Fix (VarBindingF :+: Exp E)

tm1F = Fix $ InR $ Annotation (E $ Right "annotation") $
  Fix $ InR $ Let
    (Fix $ InR $ NumLit $ E $ Left 1)
    (Fix $ InL $ BindingF ["x"] $ Fix $ InR $ Plus
      (Fix $ InL $ VarF "x")
      (Fix $ InR $ NumLit $ E $ Left 2)
      )

tm2F = Fix $ InR $ Cat
 (Fix (InR (StrLit "foo")))
 (Fix (InR (StrLit "bar")))

tm3F = Fix $ InR $ Times
  (Fix $ InR $ Len $ Fix $ InR $ StrLit "hippo")
  (Fix $ InR $ NumLit $ E $ Left 3)

evalMachinePrimitiveF
  :: Text -> Maybe (Seq (Val E (Fix (Val E))) -> Val E (Fix (Val E)))
evalMachinePrimitiveF = \case
  "add" -> Just $ \case
    NumV (E (Left x))  :< NumV (E (Left y))  :< Empty -> NumV (E (Left (x + y)))
    args                      -> error $ "bad call to add: " ++ show args
  "mul" -> Just $ \case
    NumV (E (Left x))  :< NumV (E (Left y))  :< Empty -> NumV (E (Left (x * y)))
    args                      -> error $ "bad call to mul: " ++ show args
  "cat" -> Just $ \case
    StrV (E (Right x)) :< StrV (E (Right y)) :< Empty -> StrV (E (Right (x <> y)))
    args                      -> error $ "bad call to cat: " ++ show args
  "len" -> Just $ \case
    StrV (E (Right x))                       :< Empty -> NumV (E (Left (Text.length x)))
    args                      -> error $ "bad call to len: " ++ show args

  _ -> Nothing

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Num", E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Str", E . Right <$> stringLiteral)
  ]
