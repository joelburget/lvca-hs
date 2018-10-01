{-# LANGUAGE QuasiQuotes       #-}
module Linguist.Languages.SimpleExample
  ( syntax
  -- , dynamics
  , statics
  , typingJudgement
  , E
  , tm1
  , tm2
  , dynamicTests
  , minusTests
  , completePatternTests
  , prettySyntaxChartTests
  , prettyStaticTests
  , matchesTests
  , evalTests
  , parseTests
  ) where

import           Control.Applicative                   ((<|>))
import           Control.Lens                          hiding (from, to)
import           Control.Monad.Reader
import qualified Data.Map.Strict                       as Map
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, pretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest                              hiding (char)
import           NeatInterpolation
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer            as L
import           Data.Sequence             (Seq)

import           Linguist.ParseLanguage
import           Linguist.Proceed                      (eval)
import           Linguist.Types
import           Linguist.Languages.MachineModel
import           Linguist.FunctorUtil


type E = Either Int Text

-- Chart of the language @e@. We use this for testing.
syntax :: SyntaxChart
syntax = SyntaxChart $ Map.fromList
  [ ("Typ", SortDef []
    [ Operator "num" (Arity []) "numbers"
    , Operator "str" (Arity []) "strings"
    ])
  , ("Exp", SortDef []
    [ Operator "num"   (ExternalArity "num") "numeral"
    , Operator "str"   (ExternalArity "str") "literal"
    , Operator "plus"
      (Arity ["Exp", "Exp"]) "addition"
    , Operator "times"
      (Arity ["Exp", "Exp"]) "multiplication"
    , Operator "cat"
      (Arity ["Exp", "Exp"]) "concatenation"
    , Operator "len"
      (Arity ["Exp"]) "length"
    -- TODO:
    --   Check for anything matching this that it has a binding pattern in the
    --   second slot
    , Operator "let"   (Arity ["Exp", Valence ["Exp"] "Exp"]) "definition"
    , Operator "annotation" (Arity [External "annotation", "Exp"]) "annotation"
    ])
  , ("List", SortDef ["a"]
    [ Operator "nil" (Arity []) ""
    , Operator "cons" (Arity ["a", Valence [] (SortAp "List" ["a"])]) ""
    ])
  ]

tm1, tm2 :: Term E
tm1 = Term "annotation"
  [ PrimValue (Right "annotation") -- XXX need this to be a different type
  , Term "let"
    [ PrimValue (Left 1)
    , Binding ["x"]
      (Term "plus"
        [ Var "x"
        , PrimValue (Left 2)
        ])
    ]
  ]

tm2 = Term "cat"
  [ PrimValue (Right "foo")
  , PrimValue (Right "bar")
  ]

pattern VI :: Int -> Term E
pattern VI x = PrimValue (Left x)

pattern VS :: Text -> Term E
pattern VS x = PrimValue (Right x)

pattern PatternPrimVar :: Text -> Maybe Text -> Pattern a
pattern PatternPrimVar ty name = PatternTm ty [PatternVar name]

evalMachinePrimitive :: Text -> Maybe (Seq (Term E) -> Term E)
evalMachinePrimitive = \case
  "add" -> Just $ \case
    VI x :< VI y :< Empty -> VI (x + y)
    args                  -> error $ "bad call to plus: " ++ show args
  "mul" -> Just $ \case
    VI x :< VI y :< Empty -> VI (x * y)
    _                     -> error "bad call to times"
  "cat" -> Just $ \case
    VS x :< VS y :< Empty -> VS (x <> y)
    _                     -> error "bad call to cat"
  "len" -> Just $ \case
    VS x :< Empty -> VI (Text.length x)
    _             -> error "bad call to len"
  _ -> Nothing

data TermF t e
  = Plus       e e
  | Times      e e
  | Cat        e e
  | Len        e
  | Let        e e
  | Annotation t e

data Prim
  = PrimAdd
  | PrimMul
  | PrimCat
  | PrimLen

data ValF val
  = NumV Int
  | StrV Text
  | Prim Prim [val]

-- TODO: non-erased types?
dynamics' :: DenotationChart' (PatF :+: TermF ()) (MeaningF :+: ValF)
dynamics' = DenotationChart'
  [ FixR (Plus (FixL (PatVarF (Just "n1"))) (FixL (PatVarF (Just "n2"))))
    :->
    FreeL (Eval (Pure "n1") "n1'"
      (FreeL (Eval (Pure "n2") "n2'"
        (FreeR (Prim PrimAdd [Pure "n1'", Pure "n2'"]))
      ))
    )
  , FixR (Times (FixL (PatVarF (Just "n1"))) (FixL (PatVarF (Just "n2"))))
    :->
    FreeL (Eval (Pure "n1") "n1'"
      (FreeL (Eval (Pure "n2") "n2'"
        (FreeR (Prim PrimMul [Pure "n1'", Pure "n2'"]))
      ))
    )
  , FixR (Cat (FixL (PatVarF (Just "s1"))) (FixL (PatVarF (Just "s2"))))
    :->
    FreeL (Eval (Pure "n1") "n1'"
      (FreeL (Eval (Pure "n2") "n2'"
        (FreeR (Prim PrimCat [Pure "n1'", Pure "n2'"]))
      ))
    )
  , FixR (Len (FixL (PatVarF (Just "e"))))
    :->
    FreeL (Eval (Pure "e") "e'" (FreeR (Prim PrimLen [Pure "e'"])))
  , FixR (Annotation () (FixL (PatVarF (Just "contents"))))
    :->
    Pure "contents"
  ]

dynamics :: DenotationChart E E
dynamics = mkDenotationChart _Right p1 p2 dynamics' where

  p1' :: Prism' (Pattern E) (Fix (PatF :+: TermF ()))
  p1' = patTermP p1

  p1 :: Prism' (Pattern E) (TermF () (Fix (PatF :+: TermF ())))
  p1 = prism' rtl ltr where
    rtl = \case
      Plus a b -> PatternTm "Plus"
        [ review p1' a
        , review p1' b
        ]
      Times a b -> PatternTm "Times"
        [ review p1' a
        , review p1' b
        ]
      Cat a b -> PatternTm "Cat"
        [ review p1' a
        , review p1' b
        ]
      Len a -> PatternTm "Len" [ review p1' a ]
      Let a b -> PatternTm "Let"
        [ review p1' a
        , review p1' b
        ]
      Annotation () a -> PatternTm "Annotation" [ review p1' a ]
    ltr = \case
      PatternTm "Plus"       [a, b] -> Plus <$> preview p1' a <*> preview p1' b
      PatternTm "Times"      [a, b] -> Times <$> preview p1' a <*> preview p1' b
      PatternTm "Cat"        [a, b] -> Cat <$> preview p1' a <*> preview p1' b
      PatternTm "Len"        [a]    -> Len <$> preview p1' a
      PatternTm "Let"        [a, b] -> Let <$> preview p1' a <*> preview p1' b
      PatternTm "Annotation" [a]    -> Annotation () <$> preview p1' a
      _                             -> Nothing

  p2' :: Prism' (Term E) (Free (MeaningF :+: ValF) Text)
  p2' = meaningTermP _Right p2

  p2 :: Prism' (Term E) (ValF (Free (MeaningF :+: ValF) Text))
  p2 = prism' rtl ltr where
    rtl = \case
      NumV i -> Term "NumV" [PrimValue (Left i)]
      StrV s -> Term "StrV" [PrimValue (Right s)]
      Prim PrimLen [a] -> Term "Prim" [ Term "PrimLen" [] , review p2' a ]
      Prim prim [a, b] ->
        let prim' = case prim of
              PrimAdd -> "PrimAdd"
              PrimMul -> "PrimMul"
              PrimCat -> "PrimCat"
        in Term "Prim"
             [ Term prim' []
             , review p2' a
             , review p2' b
             ]
    ltr = \case
      Term "NumV" [PrimValue (Left i)]  -> Just (NumV i)
      Term "StrV" [PrimValue (Right s)] -> Just (StrV s)
      Term "Prim" [Term "PrimLen" [], a] ->
        Prim PrimAdd . (:[]) <$> preview p2' a
      Term "Prim" [Term prim [], a, b] -> do
        prim' <- case prim of
          "PrimAdd" -> Just PrimAdd
          "PrimMul" -> Just PrimMul
          "PrimCat" -> Just PrimCat
          _ -> Nothing
        a' <- preview p2' a
        b' <- preview p2' b
        pure $ Prim prim' [a', b']
      _ -> Nothing

dynamicTests :: Test ()
dynamicTests =
  let
      n1, n2, lenStr :: Term E
      lenStr      = Term "len" [ PrimValue (Right "str") ]
      times v1 v2 = Term "times" [v1, v2]
      plus v1 v2  = Term "plus" [v1, v2]
      n1          = PrimValue (Left 1)
      n2          = PrimValue (Left 2)
      x           = PatternVar (Just "x")
      patCheck    = runMatches syntax "Exp" $ patternCheck dynamics
      env         = MatchesEnv syntax "Exp" $ Map.singleton "x" $
        PrimValue $ Left 2
  in tests
       [ expectJust $ runMatches syntax "Exp" $ matches x
         lenStr
       , expectJust $ runMatches syntax "Exp" $ matches (PatternTm "len" [x])
         lenStr
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         lenStr
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         (times n1 n2)
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         (plus n1 n2)
       , expectJust $ runMatches syntax "Exp" $ findMatch dynamics
         tm1

--        , let Just (subst, _) = findMatch syntax "Exp" dynamics tm1
--              result = applySubst subst tm1
--        , expect $ result == tm1 & ix 0 .~

       , expectJust $
         let pat = PatternTm "cat"
               [ PatternPrimVar "str" (Just "s_1")
               , PatternPrimVar "str" (Just "s_2")
               ]
         in runMatches syntax "Exp" $ matches pat tm2
       , expectJust $
         let pat :: Pattern E
             pat = PatternPrimVar "num" (Just "n_1")
             tm  = Var "x"
         in flip runReaderT env $ matches pat tm
       , expectJust $
         let pat = PatternPrimVar "num" (Just "n_2")
             tm  = VI 2
         in flip runReaderT env $ matches pat tm
       , expectJust $
         let pat = PatternTm "plus"
               [ PatternPrimVar "num" (Just "n_1")
               , PatternPrimVar "num" (Just "n_2")
               ]
             tm = Term "plus" [Var "x", VI 2]
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
      (Just (PatternUnion [PatternTm "num" [], PatternTm "str" []]))
  , expectEq'
      (runMatches syntax "Exp" completePattern)
      (Just (PatternUnion
        [ PatternTm "num"        [PatternPrimVar "num" Nothing]
        , PatternTm "str"        [PatternPrimVar "str" Nothing]
        , PatternTm "plus"       [PatternAny, PatternAny]
        , PatternTm "times"      [PatternAny, PatternAny]
        , PatternTm "cat"        [PatternAny, PatternAny]
        , PatternTm "len"        [PatternAny]
        , PatternTm "let"        [PatternAny, PatternAny]
        , PatternTm "annotation" [PatternPrimVar "annotation" Nothing, PatternAny]
        ]))
  ]

minusTests :: Test ()
minusTests = scope "minus" $
  let x = PatternVar (Just "x")
      y = PatternVar (Just "y")
      num = PatternPrimVar "num" Nothing
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
             [ PatternPrimVar "num" Nothing
             , x
             ])
           (PatternTm "plus"
             [ PatternPrimVar "num" Nothing
             , y
             ])))
         (Just PatternEmpty)


       -- This is wrong:
       -- , expect $
       --   let env = MatchesEnv syntax "Exp" $ Map.fromList
       --         -- TODO: we should be able to do this without providing values
       --         [ ("x", Right (PrimValue 2))
       --         , ("y", Right (PrimValue 2))
       --         ]
       --   in (traceShowId $ flip runReaderT env (minus
       --        (PatternTm "plus" [x, y])
       --        (PatternTm "plus"
       --          [ PatternPrimVar "num" Nothing
       --          , PatternPrimVar "num" Nothing
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
        num[num]
        str[str]
        plus(Exp; Exp)
        times(Exp; Exp)
        cat(Exp; Exp)
        len(Exp)
        let(Exp; Exp.Exp)
        annotation([annotation]; Exp)
      List a ::=
        nil
        cons(a; List a)
      Typ ::=
        num
        str
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

evalTests :: Test ()
evalTests =
  let eval' = eval "Exp" syntax dynamics _Right evalMachinePrimitive
  in tests
       [ expect $ eval' tm1 == Right (PrimValue (Left 3))
       , expect $ eval' tm2 == Right (PrimValue (Right "foobar"))
       ]

parseTests :: Test ()
parseTests =
  let primParser =
        Left <$> (L.decimal :: Parser Int) <|>
        -- TODO: better string literal
        Right <$> (char '"' >> manyTill L.charLiteral (char '"'))
      parser = standardParser primParser
      runP str = runReader (runParserT parser "(test)" str) (syntax, "Exp")
      expectParse str tm = case runP str of
        Left err       -> fail $ errorBundlePretty err
        Right parsedTm -> expectEq parsedTm tm
  in tests
  [ expectParse
      "plus(1; 2)"
      (Term "plus" [PrimValue (Left 1), PrimValue (Left 2)])
  , expectParse
      "cat(\"abc\"; \"def\")"
      (Term "cat" [PrimValue (Right "abc"), PrimValue (Right "def")])

  -- Note this doesn't check but it should still parse
  , expectParse
      "cat(\"abc\"; 1)"
      (Term "cat" [PrimValue (Right "abc"), PrimValue (Left 1)])

  , expectParse
     "let(1; x. plus(x; 2))"
     (Term "let"
       [ PrimValue (Left 1)
       , Binding ["x"]
         (Term "plus"
           [ Var "x"
           , PrimValue (Left 2)
           ])
       ])
  ]
