{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Languages.SimpleExample
  ( dynamicTests
  , minusTests
  , completePatternTests
  , prettySyntaxChartTests
  , prettyStaticTests
  , matchesTests
  -- , evalTests
  , parseTests
  , propTests

  -- currently unused:
  , tm1
  , tm3
  , typingJudgement
  ) where

import           Codec.Serialise
import           Control.Lens                          hiding (from, op, to)
import           Control.Monad.Reader
import qualified Data.Map.Strict                       as Map
import           Data.String                           (IsString(fromString))
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
  (Pretty(pretty), defaultLayoutOptions, layoutPretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest                              hiding (matches)
import           GHC.Generics                          (Generic)
import           Hedgehog                              (Gen)
import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range
import           NeatInterpolation
import           Text.Megaparsec

import           Lvca                                  hiding (statics)
import           Lvca.Types                            (matches)

import           Test.ParseTerm
import           Test.Types

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
pattern PrimValue' name x = Term name [ PrimValue (E x) ]

pattern TI :: Int -> Term E
pattern TI x = PrimValue' "Num" (Left x)

pattern TS :: Text -> Term E
pattern TS x = PrimValue' "Str" (Right x)

mkTypes (Options Nothing $ Map.fromList
  [ "Annot" :-> [t| ()   |]
  , "Int"   :-> [t| Int  |]
  , "Text"  :-> [t| Text |]
  ])
  [text|
Exp ::=
  Plus(Exp; Exp)
  Times(Exp; Exp)
  Cat(Exp; Exp)
  Len(Exp)
  Let(Exp; Exp)
  Annotation({Annot}; Exp)
  NumLit{Int}
  StrLit{Text}
  |]
mkSyntaxInstances ''Exp

_concreteSyntaxChart :: Text
_concreteSyntaxChart = [text|
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
  [text|
  Val ::=
    NumV{Int}
    StrV{Text}
  |]
mkSyntaxInstances ''Val

tm1, tm2, tm3 :: Term E
tm1 = Term "Annotation"
  [ TS "annotation"
  , Term "Let"
    [ TI 1
    , Binding ["x"] $
      Term "Plus"
        [ Var "x"
        , TI 2
        ]
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

typingJudgement :: JudgementForm
typingJudgement = JudgementForm "types" [(JIn, "Exp"), (JIn, "Typ")]

statics :: JudgementRules
statics =
  let tm -: ty = [tm, ty] :%%% "types"
  in JudgementRules
  [ ["x" -: "num", "y" -: "num"]
    :--
    ("plus" :@@ ["x", "y"]) -: "num"
  , ["x" -: "num", "y" -: "num"]
    :--
    ("times" :@@ ["x", "y"]) -: "num"
  , ["x" -: "str", "y" -: "str"]
    :--
    ("cat" :@@ ["x", "y"]) -: "str"
  , ["x" -: "str"]
    :--
    ("len" :@@ ["x"]) -: "num"
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

-- valP_round_trip_prop :: Test
-- valP_round_trip_prop = property $ do
--   x <- forAll genVal
--   let termP' :: Prism' (Term E) (Fix (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val E))
--       termP' = lambdaTermP (_Fix . _PrimValue . _Wrapped . _Right)
--   preview termP' (review termP' x) === Just x

-- pattern FixVal
--   :: Val E (Fix (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val E))
--   -> Fix        (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val E)
-- pattern FixVal x = Fix (InR (InR (InR x)))

-- -- TODO: generate VarBindingF / LambdaF as well
-- genVal :: Gen (Fix (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val E))
-- genVal = Gen.recursive Gen.choice [
--     FixVal . NumV . E . Left  <$> Gen.int Range.exponentialBounded
--   , FixVal . StrV . E . Right <$> genText
--   ] [
--     Gen.subterm2 genVal genVal (\x y -> FixApp "add" [x, y])
--   , Gen.subterm2 genVal genVal (\x y -> FixApp "mul" [x, y])
--   , Gen.subterm2 genVal genVal (\x y -> FixApp "cat" [x, y])
--   , Gen.subterm  genVal        (\x   -> FixApp "len" [x   ])
--   ]

-- matchingTermTests :: Test
-- matchingTermTests = tests
--   [ example $ tm1F === tm1
--   ]

expectJust :: Maybe a -> Test
expectJust  = unitTest . \case
  Nothing -> crash "Expected Just, found Nothing"
  Just _  -> success

dynamicTests :: Test
dynamicTests =
  let
      lenStr :: Term E
      lenStr      = Term "Len" [ TS "str" ]
      x           = PatternVar (Just "x")
      env         = MatchesEnv syntax "Exp" $ Map.singleton "x" $ VI 2
  in tests
       [ expectJust $ runMatches syntax "Exp" $ matches x
         lenStr
       , expectJust $ runMatches syntax "Exp" $ matches (PatternTm "Len" [x])
         lenStr

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

       ]

expectEq' :: Maybe (Pattern Void) -> Maybe (Pattern Void) -> Test
expectEq' a b = unitTest $ a === b

completePatternTests :: Test
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

minusTests :: Test
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

prettySyntaxChartTests :: Test
prettySyntaxChartTests = tests
  [ example $
    renderStrict (layoutPretty defaultLayoutOptions (pretty syntax))
    ===
    Text.init [text|
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
    |]
  ]

prettyStaticTests :: Test
prettyStaticTests = tests
  [ example $
    renderStrict (layoutPretty defaultLayoutOptions (pretty statics))
    ===
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

matchesTests :: Test
matchesTests = scope "matches" $
  let foo :: Term ()
      foo = Term "foo" []

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
       , example $
         (runMatches syntax "Typ" $ matches
           PatternAny
           (Binding ["x"] $ Term "num" []))
         ===
         (Just mempty :: Maybe (Subst ()))
       ]

-- evalTests :: Test
-- evalTests =
--   let run' tm expected = unitTest $ case primEval tm of
--         Left err     -> crash err
--         Right result -> result === expected
--   in tests
--        [ run' tm1F $ Fix $ NumV $ E $ Left 3
--        , run' tm2F $ Fix $ StrV "foobar"
--        , run' tm3F $ Fix $ NumV $ E $ Left 15
--        ]

parseTests :: Test
parseTests =
  let runP sty str = runParser
        (runReaderT standardParser (ParseEnv syntax "Exp" sty primParsers))
        "(test)" str
      expectParse sty str tm = scope (Text.unpack str) $ example $
        case runP sty str of
          Left err       -> crash $ errorBundlePretty err
          Right parsedTm -> parsedTm === tm
      expectNoParse sty str = scope (Text.unpack str) $ example $
        case runP sty str of
          Left _   -> success
          Right tm -> crash $ "parsed " ++ show tm
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
       , Binding ["x"] $
         Term "Plus"
           [ Var "x"
           , TI 2
           ]
       ])

  , expectParse UntaggedExternals "0" (TI 0)
  ]

propTests :: Test
propTests =
  let aGen = \case
        "Num" -> Just $ E . Left  <$> Gen.int Range.exponentialBounded
        "Str" -> Just $ E . Right <$> genText
        _     -> Nothing
  in tests
       [ scope "prop_parse_abstract_pretty" $
         prop_parse_abstract_pretty syntax "Exp" aGen primParsers

       , scope "prop_serialise_identity" $
         prop_serialise_identity syntax "Exp" aGen

       -- , scope "valP_round_trip_prop" $ property $
       --   valP_round_trip_prop
       ]

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Num", E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Str", E . Right <$> stringLiteral)
  ]
