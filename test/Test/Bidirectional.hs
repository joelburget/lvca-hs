{-# LANGUAGE PatternSynonyms #-}
module Test.Bidirectional where

import qualified Data.Map        as Map
import qualified Data.Set as Set
import Data.Text (Text)
import EasyTest

import Lvca.Bidirectional

true, false, bool :: Term

true  = Term "true"  Set.empty []
false = Term "false" Set.empty []
bool  = Term "Bool"  Set.empty []

ite :: Term -> Term -> Term -> Term
ite t1 t2 t3 = Term "ite" Set.empty [ t1, t2, t3 ]

lam :: Text -> Term -> Term
lam x t = Term "lam" (Set.singleton x) [ t ]

app, arr, annot :: Term -> Term -> Term
app   t1 t2 = Term "app"   Set.empty [ t1, t2 ]
arr   t1 t2 = Term "arr"   Set.empty [ t1, t2 ]
annot tm ty = Term "annot" Set.empty [ tm, ty ]

infix 0 .--
infix 4 .=>
infix 4 .<=

(.--) :: [(Ctx, TypingClause)] -> TypingClause -> Rule
hyps .-- conc = Rule hyps conc

(.=>) :: Term -> Term -> TypingClause
tm .=> ty = InferenceRule (tm :=> ty)

(.<=) :: Term -> Term -> TypingClause
tm .<= ty = CheckingRule (tm :<= ty)

env :: Env
env =
  let t   = Var "t"
      ty  = Var "ty"
      ty1 = Var "ty1"
      ty2 = Var "ty2"
      t1  = Var "t1"
      t2  = Var "t2"
      t3  = Var "t3"
      gamma = Map.empty
  in Env
       [ []
         .--
         true  .=> bool
       , []
         .--
         false .=> bool

       , [ (gamma, t1 .<= bool)
         , (gamma, t2 .<= ty)
         , (gamma, t3 .<= ty)
         ]
         .--
         ite t1 t2 t3 .<= ty

       , [ (Map.singleton "x" ty1, t .<= ty2)
         ]
         .--
         lam "x" t .<= arr ty1 ty2

       , [ (gamma, t1 .=> arr ty1 ty2)
         , (gamma, t2 .<= ty1)
         ]
         .--
         app t1 t2 .=> ty2

       -- important: this rule must go last otherwise it will subsume all other
       -- checking rules
       , [ (gamma, t .=> ty) ]
         .--
         t .<= ty
       , [ (gamma, t .<= ty) ]
         .--
         annot t ty .=> ty
       ]
     Map.empty

checkingTests :: Test
checkingTests = scope "bidirectional" $ tests
  [ scope "1" $ example $ runCheck env (infer true)                       === Just bool
  , scope "2" $ example $ runCheck env (infer false)                      === Just bool
  , scope "3" $ example $ runCheck env (check (false :< bool))            === Just ()
  , scope "4" $ example $ runCheck env (check (annot false bool :< bool)) === Just ()
  , scope "5" $ example $ runCheck env (infer (annot false bool))         === Just bool
  , scope "6" $ example $
    runCheck env (check (idTm :< arr bool bool)) === Just ()
  , scope "7" $ example $
    runCheck env (infer idTm') === Just (arr bool bool)
  ]

idTm = lam "x" (Var "x")
idTm' = annot idTm (arr bool bool)
