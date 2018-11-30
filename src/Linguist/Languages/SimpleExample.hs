{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
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
  , propTests

  , eval'
  ) where

import           Control.Lens                          hiding (from, to)
import           Control.Monad.Reader
import qualified Data.Map.Strict                       as Map
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec
import           Data.Sequence                         (Seq)
import           Data.Diverse.Lens.Which
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range

import qualified Linguist.ParseDenotationChart         as PD
import           Linguist.ParseLanguage
import           Linguist.Proceed
import           Linguist.Types
import           Linguist.Languages.MachineModel
import           Linguist.FunctorUtil
import           Linguist.ParseUtil
import           Linguist.Util                         (forceRight)


newtype E = E (Either Int Text)
  deriving (Eq, Show)

makeWrapped ''E

instance AsFacet Text E where
  facet = _Wrapped . _Right

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

tm1, tm2 :: Term E
tm1 = Term "Annotation"
  [ TS "annotation" -- XXX need this to be a different type
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

evalMachinePrimitive :: Text -> Maybe (Seq (Term E) -> Term E)
evalMachinePrimitive = \case
  "add" -> Just $ \case
    TI x :< TI y :< Empty -> VI (x + y)
    args                  -> error $ "bad call to plus: " ++ show args
  "mul" -> Just $ \case
    TI x :< TI y :< Empty -> VI (x * y)
    _                     -> error "bad call to times"
  "cat" -> Just $ \case
    TS x :< TS y :< Empty -> VS (x <> y)
    _                     -> error "bad call to cat"
  "len" -> Just $ \case
    TS x :< Empty -> VI (Text.length x)
    _             -> error "bad call to len"
  _ -> Nothing

data TermF t e
  = Plus       !e !e
  | Times      !e !e
  | Cat        !e !e
  | Len        !e
  | Let        !e !e
  | Annotation !t !e

data ValF val
  = NumV !Int
  | StrV !Text
  | PrimAdd !val !val
  | PrimMul !val !val
  | PrimCat !val !val
  | PrimLen !val

-- TODO: non-erased types?
dynamicsF :: DenotationChart' (PatF :+: TermF ()) (MeaningF :+: ValF)
dynamicsF = DenotationChart'
  [ FixR (Plus (FixL (PatVarF (Just "n1"))) (FixL (PatVarF (Just "n2"))))
    :->
    FreeL (Eval (Pure "n1") "n1'"
      (FreeL (Eval (Pure "n2") "n2'"
        (FreeR (PrimAdd (Pure "n1'") (Pure "n2'")))
      ))
    )
  , FixR (Times (FixL (PatVarF (Just "n1"))) (FixL (PatVarF (Just "n2"))))
    :->
    FreeL (Eval (Pure "n1") "n1'"
      (FreeL (Eval (Pure "n2") "n2'"
        (FreeR (PrimMul (Pure "n1'") (Pure "n2'")))
      ))
    )
  , FixR (Cat (FixL (PatVarF (Just "s1"))) (FixL (PatVarF (Just "s2"))))
    :->
    FreeL (Eval (Pure "n1") "n1'"
      (FreeL (Eval (Pure "n2") "n2'"
        (FreeR (PrimCat (Pure "n1'") (Pure "n2'")))
      ))
    )
  , FixR (Len (FixL (PatVarF (Just "e"))))
    :->
    FreeL (Eval (Pure "e") "e'" (FreeR (PrimLen (Pure "e'"))))
  , FixR (Annotation () (FixL (PatVarF (Just "contents"))))
    :->
    Pure "contents"
  ]

dynamics :: DenotationChart E E
dynamics = mkDenotationChart (_Wrapped . _Right) p1 p2 dynamicsF

dynamicsT :: Text
dynamicsT = [text|
  [[ Plus(n1; n2) ]] = Eval([[ n1 ]]; n1'.
                         Eval([[ n2 ]]; n2'.
                           PrimApp(Str({add}); n1'; n2')))
  [[ Times(n1; n2) ]] = Eval([[ n1 ]]; n1'.
                          Eval([[ n2 ]]; n2'.
                            PrimApp(Str({mul}); n1'; n2')))
  [[ Cat(n1; n2) ]]   = Eval([[ n1 ]]; n1'.
                          Eval([[ n2 ]]; n2'.
                            PrimApp(Str({cat}); n1'; n2')))
  [[ Len(e) ]]                  = Eval([[ e ]]; e1'. PrimApp(Str({len}); e1'))
  [[ Let(e; v. body) ]]         = Eval([[ e ]]; e'.
                                    Eval(
  // XXX change x to v and this no longer works -- accidental capture!
                                      [[ body ]][e' / x];
                                      body'. body'
                                    )
                                  )
  [[ Annotation(_; contents) ]] = Eval([[ contents ]]; contents'. contents')
  |]

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left <$> intLiteral
  , Right "add" <$ symbol "add"
  , Right "mul" <$ symbol "mul"
  , Right "cat" <$ symbol "cat"
  , Right "len" <$ symbol "len"
  ]

dynamics' :: Either (ParseErrorBundle Text Void) (DenotationChart E E)
dynamics' = runParser (PD.parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" dynamicsT

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
p2' = meaningTermP (_Wrapped . _Right) p2

p2 :: Prism' (Term E) (ValF (Free (MeaningF :+: ValF) Text))
p2 = prism' rtl ltr where
  rtl = \case
    NumV i      -> VI i
    StrV s      -> VS s
    PrimLen a   -> Term "PrimLen" [ review p2' a ]
    PrimAdd a b -> Term "PrimAdd" [ review p2' a , review p2' b ]
    PrimMul a b -> Term "PrimMul" [ review p2' a , review p2' b ]
    PrimCat a b -> Term "PrimCat" [ review p2' a , review p2' b ]
  ltr = \case
    VI i                  -> Just (NumV i)
    VS s                  -> Just (StrV s)
    Term "PrimLen" [a]    -> PrimLen <$> preview p2' a
    Term "PrimAdd" [a, b] -> PrimAdd <$> preview p2' a <*> preview p2' b
    Term "PrimMul" [a, b] -> PrimMul <$> preview p2' a <*> preview p2' b
    Term "PrimCat" [a, b] -> PrimCat <$> preview p2' a <*> preview p2' b
    _                     -> Nothing

dynamicTests :: Test ()
dynamicTests =
  let
      n1, n2, lenStr :: Term E
      lenStr      = Term "Len" [ TS "str" ]
      times v1 v2 = Term "Times" [v1, v2]
      plus v1 v2  = Term "Plus" [v1, v2]
      n1          = TI 1
      n2          = TI 2
      x           = PatternVar (Just "x")
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
eval' = eval $ mkEvalEnv "Exp" syntax (forceRight dynamics') evalMachinePrimitive Just

evalTests :: Test ()
evalTests = tests
  [ expectEq (eval' tm1) (Right (VI 3))
  -- , expectEq (eval' tm2) (Right (VS "foobar"))
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
propTests = tests
  [ testProperty $ prop_parse_pretty syntax "Exp"
     (\case
       "Num" -> Just (E . Left  <$> Gen.int (Range.exponentialBounded))
       "Str" -> Just (E . Right <$>
         Gen.text (Range.exponential 0 5000) Gen.unicode)
       _     -> Nothing
     )
     primParsers
  ]
