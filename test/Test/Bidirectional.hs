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
  let t    = Var "t"
      tau  = Var "tau"
      tau1 = Var "tau1"
      tau2 = Var "tau2"
      t1   = Var "t1"
      t2   = Var "t2"
      t3   = Var "t3"
  in Env
       [ []
         .--
         true  .=> bool
       , []
         .--
         false .=> bool

       , [ t .=> tau ]
         .--
         t .<= tau
       , [ t .<= tau ]
         .--
         annot t tau .=> tau

       , [ t1 .<= bool
         , t2 .<= tau
         , t3 .<= tau
         ]
         .--
         ite t1 t2 t3 .<= tau

       , [ t .<= tau2 -- XXX must include context
         ]
         .--
         lam t .<= arr tau1 tau2

       , [ t1 .=> arr tau1 tau2
         , t2 .<= tau1
         ]
         .--
         app t1 t2 .=> tau2
       ]

checkingTests :: Test
checkingTests = scope "bidirectional" $ tests
  [ scope "1" $ example $ runCheck rules (infer true)                       === Just bool
  , scope "2" $ example $ runCheck rules (infer false)                      === Just bool
  , scope "3" $ example $ runCheck rules (check (false :< bool))            === Just ()
  , scope "4" $ example $ runCheck rules (check (annot false bool :< bool)) === Just ()
  , scope "5" $ example $ runCheck rules (infer (annot false bool))         === Just bool
  ]
