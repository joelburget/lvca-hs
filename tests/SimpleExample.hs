
module Test.SimpleExample
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
  ) where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.CPS
import qualified Data.Map.Strict                       as Map
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Hedgehog                              (Property, property, (===), forAll, Gen)
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range

import           Linguist.Proceed                      hiding (matches, findMatch)
import           Linguist.Util                         (forceRight)

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
  :: ExpF () E (Fix (PatVarF :+: ExpF () E))
  -> Fix            (PatVarF :+: ExpF () E)
pattern FixExp x = Fix (InR x)

genPat :: Gen (Fix (PatVarF :+: ExpF () E))
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
  , Gen.subterm  genPat        (     FixExp . Annotation ())
  ]

pattern FixVal
  :: ValF E (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF E))
  -> Fix         (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF E)
pattern FixVal x = Fix (InR (InR (InR x)))

pattern FixApp
  :: Text
  -> [ Fix         (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF E) ]
  -> Fix           (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF E)
pattern FixApp name tms = Fix (InR (InR (InL (PrimApp name tms))))

-- TODO: generate VarBindingF / MachineF as well
genVal :: Gen (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF E))
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
      lenStr      = Fix $ Term "Len"   [ TS "str" ]
      times v1 v2 = Fix $ Term "Times" [ v1, v2   ]
      plus v1 v2  = Fix $ Term "Plus"  [ v1, v2   ]
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
       -- , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
       --   lenStr
       -- , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
       --   (times n1 n2)
       -- , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
       --   (plus n1 n2)
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
           (Fix $ Binding ["x"] $ Fix $ Term "num" []))
         ==
         (Just (Subst Map.empty [("y", "x")])
           :: Maybe (Subst ()))
       ]

evalTests :: Test ()
evalTests =
  let run tm expected = case evalF tm of
        (Left err,     _logs) -> fail err
        (Right result, _logs) -> expectEq result expected
  in tests
       [ run tm1F $ Fix $ NumV $ E $ Left 3
       , run tm2F $ Fix $ StrV "foobar"
       , run tm3F $ Fix $ NumV $ E $ Left 15
       ]

translateTests :: Test ()
translateTests =
  let run tm expected = case runWriter (runMaybeT (translate dynamicsF tm)) of
        (Nothing,     _logs) -> fail "couldn't find match"
        (Just result, _logs) -> expectEq result expected
  in tests
       [ run (Fix $ InR $ NumLit $ E $ Left 2) $
           Fix $ InR $ InR $ NumV $ Right $ E $ Left 2

       , run (Fix $ InR $ StrLit $ E $ Right "str") $
           Fix $ InR $ InR $ StrV $ Right $ E $ Right "str"

       , run tm1F $
           Fix $ InR $ InL $ App
             (Fix $ InR $ InL $ Lam $
               Fix $ InL $ BindingF ["x"] $
                 Fix $ InR $ InL $ PrimApp "add"
                   [ Fix $ InL $ VarF "x"
                   , Fix $ InR $ InR $ NumV $ Right $ E $ Left 2
                   ])
             (Fix $ InR $ InR $ NumV $ Right $ E $ Left 1)

       , run tm2F $
           Fix $ InR $ InL $ PrimApp "cat"
             [ Fix $ InR $ InR $ StrV $ Right $ E $ Right "foo"
             , Fix $ InR $ InR $ StrV $ Right $ E $ Right "bar"
             ]

       , run tm3F $
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

dynamics :: DenotationChart Text (Either Text Text)
dynamics = mkDenotationChart patP valP dynamicsF

evalF
  :: Fix (VarBindingF :+: ExpF () E)
  -> (Either String (Fix (ValF E)), Seq Text)
evalF = eval (EvalEnv Map.empty evalMachinePrimitiveF) dynamicsF

-- TODO: non-erased types?
dynamicsF :: DenotationChart' (ExpF () Text) (MachineF :+: ValF Text)
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

  , Fix (InR (Annotation () (Fix (InL (PatVarF (Just "contents"))))))
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
      -> ExpF () Text (Fix (PatVarF :+: ExpF () Text)))
    -> Fix (PatVarF :+: ExpF () Text)
  matchop op = FixIn $ op
    (Fix $ InL $ PatVarF $ Just "n1")
    (Fix $ InL $ PatVarF $ Just "n2")

  rhsop
    :: (f ~ Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF Text))
    => Text -> f
  rhsop name = Fix $ InR $ InR $ InL $ PrimApp name
    [ Fix $ InR $ InL $ MeaningOf "n1"
    , Fix $ InR $ InL $ MeaningOf "n2"
    ]

explicitPatP :: forall a. Prism' (Pattern a) (Fix (PatVarF :+: ExpF () a))
explicitPatP = prism' rtl ltr where
  rtl = \case
    Fix (InL pat) -> review patVarP pat
    Fix (InR pat) -> review patP'   pat
  ltr tm = msum
    [ Fix . InL <$> preview patVarP tm
    , Fix . InR <$> preview patP'   tm
    ]

  p :: Prism' (Pattern a) a
  p = _PatternPrimVal . _Just

  patP' :: Prism' (Pattern a) (ExpF () a (Fix (PatVarF :+: ExpF () a)))
  patP' = prism' rtl' ltr' where
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

valP :: forall a. Prism'
  (Term (Either Text a))
  (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF a))
valP = prism' rtl ltr where
  rtl :: Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF a) -> Term (Either Text a)
  rtl = \case
    Fix (InL tm')             -> review (varBindingP valP) tm'
    Fix (InR (InL tm'))       -> review meaningP           tm'
    Fix (InR (InR (InL tm'))) -> review (machineP valP)    tm'
    Fix (InR (InR (InR tm'))) -> review valP'              tm'

  ltr :: Term (Either Text a) -> Maybe (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF a))
  ltr tm = msum
    [ Fix . InL             <$> preview (varBindingP valP) tm
    , Fix . InR . InL       <$> preview meaningP           tm
    , Fix . InR . InR . InL <$> preview (machineP valP)    tm
    , Fix . InR . InR . InR <$> preview valP'              tm
    ]

  valP' :: Prism'
    (Term (Either Text a))
    (ValF a (Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: ValF a)))
  valP' = prism' rtl' ltr' where
    rtl' = \case
      NumV i      -> Fix $ Term "NumV" [ Fix $ PrimValue $ Right i ]
      StrV s      -> Fix $ Term "StrV" [ Fix $ PrimValue $ Right s ]
    ltr' (Fix tm) = case tm of
      Term "NumV" [ Fix (PrimValue (Right i)) ] -> Just $ NumV i
      Term "StrV" [ Fix (PrimValue (Right s)) ] -> Just $ StrV s
      _                       -> Nothing
