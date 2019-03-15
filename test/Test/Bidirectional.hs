{-# LANGUAGE PatternSynonyms #-}
module Test.Bidirectional where

import Prelude  hiding (Bool(..))
import EasyTest

import Lvca.Bidirectional

pattern True, False, Bool :: Term'

pattern True  = Term' "true"  []
pattern False = Term' "false" []
pattern Bool  = Term' "Bool"  []

pattern Ite :: Term' -> Term' -> Term' -> Term'
pattern Ite t1 t2 t3 = Term' "ite" [ t1, t2, t3 ]

pattern Lam :: Term' -> Term'
pattern Lam t = Term' "lam" [ t ]

pattern App, Arr, Annot :: Term' -> Term' -> Term'
pattern App   t1 t2 = Term' "app"   [ t1, t2 ]
pattern Arr   t1 t2 = Term' "arr"   [ t1, t2 ]
pattern Annot tm ty = Term' "annot" [ tm, ty ]

rules :: Env
rules =
  let t    = Var' "t"
      tau  = Var' "tau"
      tau1 = Var' "tau1"
      tau2 = Var' "tau2"
      t1   = Var' "t1"
      t2   = Var' "t2"
      t3   = Var' "t3"
  in Env
       [ Rule [] $ InferenceRule $ True  :=> Bool
       , Rule [] $ InferenceRule $ False :=> Bool

       , Rule [InferenceRule $ t :=> tau ] $
         CheckingRule $ t :<= tau
       , Rule [CheckingRule $ t :<= tau ] $
         InferenceRule $ Annot t tau :=> tau

       , Rule
         [ CheckingRule $ t1 :<= Bool
         , CheckingRule $ t2 :<= tau
         , CheckingRule $ t3 :<= tau
         ] $ CheckingRule $ Ite t1 t2 t3 :<= tau

       , Rule
         [ CheckingRule $ t :<= tau2 -- XXX must include context
         ] $ CheckingRule $ Lam t :<= Arr tau1 tau2

       , Rule
         [ InferenceRule $ t1 :=> Arr tau1 tau2
         , CheckingRule $ t2 :<= tau1
         ] $ InferenceRule $ App t1 t2 :=> tau2
       ]

checkingTests :: Test
checkingTests = scope "bidirectinal" $ tests
  [
  ]
