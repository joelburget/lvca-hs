{-# LANGUAGE PatternSynonyms #-}
module Test.Bidirectional where

import EasyTest

import Lvca.Bidirectional hiding (rules)

true, false, bool :: Term

true  = Term "true"  []
false = Term "false" []
bool  = Term "Bool"  []

ite :: Term -> Term -> Term -> Term
ite t1 t2 t3 = Term "ite" [ t1, t2, t3 ]

lam :: Term -> Term
lam t = Term "lam" [ t ]

app, arr, annot :: Term -> Term -> Term
app   t1 t2 = Term "app"   [ t1, t2 ]
arr   t1 t2 = Term "arr"   [ t1, t2 ]
annot tm ty = Term "annot" [ tm, ty ]

infix 0 .--
infix 4 .=>
infix 4 .<=

(.--) :: [TypingClause] -> TypingClause -> Rule
hyps .-- conc = Rule hyps conc

(.=>) :: Term -> Term -> TypingClause
tm .=> ty = InferenceRule (tm :=> ty)

(.<=) :: Term -> Term -> TypingClause
tm .<= ty = CheckingRule (tm :<= ty)

rules :: Env
rules =
  let t   = Var "t"
      ty  = Var "ty"
      ty1 = Var "ty1"
      ty2 = Var "ty2"
      t1  = Var "t1"
      t2  = Var "t2"
      t3  = Var "t3"
  in Env
       [ []
         .--
         true  .=> bool
       , []
         .--
         false .=> bool

       , [ t .=> ty ]
         .--
         t .<= ty
       , [ t .<= ty ]
         .--
         annot t ty .=> ty

       , [ t1 .<= bool
         , t2 .<= ty
         , t3 .<= ty
         ]
         .--
         ite t1 t2 t3 .<= ty

       , [ t .<= ty2 -- XXX must include context
         ]
         .--
         lam t .<= arr ty1 ty2

       , [ t1 .=> arr ty1 ty2
         , t2 .<= ty1
         ]
         .--
         app t1 t2 .=> ty2
       ]

checkingTests :: Test
checkingTests = scope "bidirectional" $ tests
  [ scope "1" $ example $ runCheck rules (infer true)                       === Just bool
  , scope "2" $ example $ runCheck rules (infer false)                      === Just bool
  , scope "3" $ example $ runCheck rules (check (false :< bool))            === Just ()
  , scope "4" $ example $ runCheck rules (check (annot false bool :< bool)) === Just ()
  , scope "5" $ example $ runCheck rules (infer (annot false bool))         === Just bool
  ]
