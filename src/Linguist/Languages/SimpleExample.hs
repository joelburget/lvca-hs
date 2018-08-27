{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.Languages.SimpleExample
  ( syntax
  , dynamics
  , statics
  , typingJudgement
  , E
  , tm1
  , tm2
  , dynamicTests
  , minusTests
  , mkCompletePatternTests
  , prettySyntaxChartTests
  , prettyStaticTests
  , matchesTests
  , evalTests
  ) where

import           Control.Lens          hiding (from, to)
import           Control.Monad.Reader
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Text.Prettyprint.Doc (pretty, layoutPretty, defaultLayoutOptions)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           EasyTest

import           Linguist.Proceed      (eval)
import           Linguist.Types


type E = Either Int Text

-- Chart of the language @e@. We use this for testing.
syntax :: SyntaxChart
syntax = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    [ Operator "num" (Arity []) "numbers"
    , Operator "str" (Arity []) "strings"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "num"   (External "num") "numeral"
    , Operator "str"   (External "str") "literal"
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
    ])
  ]

tm1, tm2 :: Term E
tm1 = Term "let"
  [ PrimValue (Left 1)
  , Binding ["x"]
    (Term "plus"
      [ Var "x"
      , PrimValue (Left 2)
      ])
  ]
tm2 = Term "cat"
  [ PrimValue (Right "foo")
  , PrimValue (Right "bar")
  ]

pattern VI :: Int -> Term E
pattern VI x = PrimValue (Left x)

pattern VS :: Text -> Term E
pattern VS x = PrimValue (Right x)

dynamics :: DenotationChart E
dynamics = DenotationChart
  -- [ (PatternVar (Just "x",) Variable "x")
  [ (PatternPrimVal "Exp" "num", Value)
  , (PatternPrimVal "Exp" "str", Value)
  , (PatternTm "plus" [PatternPrimVal "num" "n_1", PatternPrimVal "num" "n_2"],
    CallForeign $ \case
      VI x :< VI y :< Empty -> VI (x + y)
      args -> error $ "bad call to plus: " ++ show args)
  , (PatternTm "times" [PatternPrimVal "num" "n_1", PatternPrimVal "num" "n_2"],
    CallForeign $ \case
      VI x :< VI y :< Empty -> VI (x * y)
      _ -> error "bad call to times")
  , (PatternTm "cat" [PatternPrimVal "str" "s_1", PatternPrimVal "str" "s_2"],
    CallForeign $ \case
      VS x :< VS y :< Empty -> VS (x <> y)
      _ -> error "bad call to cat")
  , (PatternTm "len" [PatternVar (Just "e")],
    CallForeign $ \case
      VS x :< Empty -> VI (Text.length x)
      _ -> error "bad call to len")
  , (PatternTm "let"
    [PatternVar (Just "e_1"), BindingPattern ["v"] (PatternVar (Just "e_2"))],
    BindIn [("v", "e_1")] "e_2")
  ]

typingJudgement :: JudgementForm
typingJudgement = JudgementForm "types" [(In, "Exp"), (In, "Typ")]

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
               [PatternPrimVal "str" "s_1", PatternPrimVal "str" "s_2"]
         in runMatches syntax "Exp" $ matches pat tm2
       , expectJust $
         let pat = PatternPrimVal "num" "n_1"
             tm  = Var "x"
         in flip runReaderT env $ matches pat tm
       , expectJust $
         let pat = PatternPrimVal "num" "n_2"
             tm  = VI 2
         in flip runReaderT env $ matches pat tm
       , expectJust $
         let pat = PatternTm "plus"
               [PatternPrimVal "num" "n_1", PatternPrimVal "num" "n_2"]
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

mkCompletePatternTests :: Test ()
mkCompletePatternTests = scope "completePattern" $ tests
  [ expect $
      runMatches syntax "Typ" completePattern
      ==
      Just (PatternUnion [PatternTm "num" [], PatternTm "str" []])
  , expect $
      runMatches syntax "Exp" completePattern
      ==
      Just (PatternUnion
        [ PatternPrimVal "Exp" "num"
        , PatternPrimVal "Exp" "str"
        , PatternTm "plus"  [PatternAny, PatternAny]
        , PatternTm "times" [PatternAny, PatternAny]
        , PatternTm "cat"   [PatternAny, PatternAny]
        , PatternTm "len"   [PatternAny]
        , PatternTm "let"   [PatternAny, PatternAny]
        ])
  ]

minusTests :: Test ()
minusTests = scope "minus" $
  let x = PatternVar (Just "x")
      y = PatternVar (Just "y")
      num = PatternPrimVal "Exp" "num"
      any' = PatternAny
  in tests
       [ expect $ runMatches undefined undefined (minus x x) == Just PatternEmpty
       , expect $ runMatches undefined undefined (minus x y) == Just PatternEmpty
       , expect $ runMatches undefined undefined (minus x any') == Just PatternEmpty
       -- , expect $ runMatches undefined undefined (minus any' x) == Just PatternEmpty
       , expect $ runMatches undefined undefined (minus any' any') == Just PatternEmpty
       , expect $
         runMatches syntax "Typ" (minus (PatternTm "num" []) (PatternTm "num" []))
         ==
         Just PatternEmpty
       , expect $ runMatches syntax "Exp" (minus num num) == Just PatternEmpty
       , expect $
         runMatches syntax "Exp" (minus
           (PatternTm "plus"
             [ PatternPrimVal "Exp" "num"
             , x
             ])
           (PatternTm "plus"
             [ PatternPrimVal "Exp" "num"
             , y
             ]))
         ==
         Just PatternEmpty


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
       --          [ PatternPrimVal "Exp" "num"
       --          , PatternPrimVal "Exp" "num"
       --          ])))
       --        ==
       --        Just PatternEmpty
       ]

prettySyntaxChartTests :: Test ()
prettySyntaxChartTests = tests
  [ expect $
    renderStrict (layoutPretty defaultLayoutOptions (pretty syntax))
    ==
    Text.intercalate "\n"
    [ "Exp ::="
    , "  num: num"
    , "  str: str"
    , "  plus: (Exp; Exp)"
    , "  times: (Exp; Exp)"
    , "  cat: (Exp; Exp)"
    , "  len: (Exp)"
    , "  let: (Exp; Exp.Exp)"
    , "Typ ::="
    , "  num"
    , "  str"
    ]
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
  let
  in tests
       [ expect $ eval "Exp" syntax dynamics tm1 == Right (PrimValue (Left 3))
       , expect $ eval "Exp" syntax dynamics tm2 == Right (PrimValue (Right "foobar"))
       ]
