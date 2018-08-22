{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.SimpleExample where

import           Control.Lens         hiding (from, to)
import           Control.Monad.Reader
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           EasyTest

import           Linguist.Types


type E = Either Int Text

tm1, tm2 :: Term E
tm1 = Term "let"
  [ Return (PrimValue (Left 1))
  , Binding ["x"]
    (Term "plus"
      [ Var "x"
      , Return (PrimValue (Left 2))
      ])
  ]
tm2 = Term "cat"
  [ Return (PrimValue (Right "foo"))
  , Return (PrimValue (Right "bar"))
  ]

pattern VI :: Int -> Value E
pattern VI x = PrimValue (Left x)

pattern VS :: Text -> Value E
pattern VS x = PrimValue (Right x)

denotation :: DenotationChart E
denotation = DenotationChart
  -- [ (PatternVar (Just "x",) Variable "x")
  [ (PatternPrimVal "Exp" "num", Value)
  , (PatternPrimVal "Exp" "str", Value)
  , (PatternTm "plus" [PatternPrimVal "num" "n_1", PatternPrimVal "num" "n_2"],
    CallForeign $ \case
      VI x :< VI y :< Empty -> VI (x + y)
      _ -> error "bad call to plus")
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
    [PatternVar (Just "e_1"), BindingPattern ["x"] (PatternVar (Just "e_2"))],
    BindIn "x" "e_1" "e_2")
  ]

denotationTests :: Test ()
denotationTests =
  let
      n1, n2, lenStr :: Term E
      lenStr = Term "len" [ Return (PrimValue (Right "str")) ]
      times v1 v2 = Term "times" [v1, v2]
      plus v1 v2 = Term "plus" [v1, v2]
      n1 = Return (PrimValue (Left 1))
      n2 = Return (PrimValue (Left 2))
      x = PatternVar (Just "x")
      -- y = PatternVar (Just "y")
      patCheck = runMatches eChart "Exp" $ patternCheck denotation
      env = MatchesEnv eChart "Exp" (Map.singleton "x" (Right (PrimValue (Left 2))))
  in tests
       [ expectJust $ runMatches eChart "Exp" $ matches x
         (Left lenStr)
       , expectJust $ runMatches eChart "Exp" $ matches (PatternTm "len" [x])
         (Left lenStr)
       , expectJust $ runMatches eChart "Exp" $ findMatch denotation
         (Left lenStr)
       , expectJust $ runMatches eChart "Exp" $ findMatch denotation
         (Left (times n1 n2))
       , expectJust $ runMatches eChart "Exp" $ findMatch denotation
         (Left (plus n1 n2))
       , expectJust $ runMatches eChart "Exp" $ findMatch denotation
         (Left tm1)

--        , let Just (subst, _) = findMatch eChart "Exp" denotation tm1
--              result = applySubst subst tm1
--        , expect $ result == tm1 & ix 0 .~

       , expectJust $
         let pat = PatternTm "cat"
               [PatternPrimVal "str" "s_1", PatternPrimVal "str" "s_2"]
         in runMatches eChart "Exp" $ matches pat (Left tm2)
       , expectJust $
         let pat = PatternPrimVal "num" "n_1"
             tm  = Var "x"
         in flip runReaderT env $ matches pat (Left tm)
       , expectJust $
         let pat = PatternPrimVal "num" "n_2"
             tm  = Return (VI 2)
         in flip runReaderT env $ matches pat (Left tm)
       , expectJust $
         let pat = PatternTm "plus"
               [PatternPrimVal "num" "n_1", PatternPrimVal "num" "n_2"]
             tm = Term "plus" [Var "x", Return (VI 2)]
         in flip runReaderT env $ matches pat (Left tm)

       , expect $
         fmap isComplete patCheck
         ==
         Just False

--        , expect $ fmap hasRedundantPat patCheck
--          ==
--          Just True

       ]
