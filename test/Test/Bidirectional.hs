{-# LANGUAGE PatternSynonyms #-}
module Test.Bidirectional where

import           Data.Map     (Map)
import qualified Data.Map     as Map
import Data.Text (Text)
import EasyTest

import Lvca.Bidirectional

true, false, bool :: Term

true  = Term "true"  []
false = Term "false" []
bool  = Term "Bool"  []

ite :: Term -> Term -> Term -> Term
ite t1 t2 t3 = Term "ite" [ Scope [] t1, Scope [] t2, Scope [] t3 ]

lam :: Text -> Term -> Term
lam x t = Term "lam" [ Scope [x] t ]

app, arr, annot :: Term -> Term -> Term
app   t1 t2 = Term "app"   [ Scope [] t1, Scope [] t2 ]
arr   t1 t2 = Term "arr"   [ Scope [] t1, Scope [] t2 ]
annot tm ty = Term "annot" [ Scope [] tm, Scope [] ty ]

bnot :: Term -> Term
bnot tm = ite tm false true

idTm, idTm', notTm, notTm' :: Term
idTm = lam "x" (Bound 0)
idTm' = annot idTm b2b
notTm = lam "x" (bnot (Bound 0))
notTm' = annot notTm b2b

b2b :: Term
b2b = arr bool bool

infix 1 .--
infix 1 |-
infix 4 .=>
infix 4 .<=

(.--) :: [(Map Text Term, TypingClause)] -> TypingClause -> Rule
hyps .-- conc = Rule hyps conc

(.=>) :: Term -> Term -> TypingClause
tm .=> ty = InferenceRule (tm :=> ty)

(.<=) :: Term -> Term -> TypingClause
tm .<= ty = CheckingRule (tm :<= ty)

(|-) :: Map Text Term -> TypingClause -> (Map Text Term, TypingClause)
(|-) = (,)

env :: Env
env =
  let t   = Free "t"
      ty  = Free "ty"
      ty1 = Free "ty1"
      ty2 = Free "ty2"
      t1  = Free "t1"
      t2  = Free "t2"
      t3  = Free "t3"
      gamma = Map.empty
  in Env
       [ []
         .--
         true  .=> bool
       , []
         .--
         false .=> bool

       , [ gamma |- t1 .<= bool
         , gamma |- t2 .<= ty
         , gamma |- t3 .<= ty
         ]
         .--
         ite t1 t2 t3 .<= ty

       , [ Map.singleton "x" ty1 |- t .<= ty2
         ]
         .--
         lam "x" t .<= arr ty1 ty2

       , [ gamma |- t1 .=> arr ty1 ty2
         , gamma |- t2 .<= ty1
         ]
         .--
         app t1 t2 .=> ty2

       , [ gamma |- t .<= ty ]
         .--
         annot t ty .=> ty

       -- important: this rule must go last otherwise it will subsume all other
       -- rules
       , [ gamma |- t .=> ty ]
         .--
         t .<= ty
       ]
       Map.empty

checkingTests :: Test
checkingTests = tests
  [ scope "matching" $ example $
    matchSchemaVars (Term "lam" [ Scope ["x"] (Free "t") ]) (lam "x" (Bound 0))
    ===
    Just (Map.singleton "t" (Scope ["x"] (Bound 0)))
  , scope "1" $ example $ runCheck env (infer true)                       === Right bool
  , scope "2" $ example $ runCheck env (infer false)                      === Right bool
  , scope "3" $ example $ runCheck env (check (false :< bool))            === Right ()
  , scope "4" $ example $ runCheck env (check (annot false bool :< bool)) === Right ()
  , scope "5" $ example $ runCheck env (infer (annot false bool))         === Right bool
  , scope "6" $ example $
    runCheck env (check (idTm :< b2b))                 === Right ()
  , scope "7" $ example $
    runCheck env (infer idTm')                         === Right b2b
  , scope "8" $ example $
    runCheck env (infer (app idTm' true))              === Right bool
  , scope "9" $ example $
    runCheck env (infer (app idTm' (app idTm' true)))  === Right bool
  , scope "10" $ example $
    runCheck env (check (ite true true false :< bool)) === Right ()
  , scope "11" $ example $
    runCheck env (check (ite true true false :< bool)) === Right ()
  , scope "12" $ example $
    runCheck env (check (notTm :< b2b))                === Right ()
  , scope "12" $ example $
    runCheck env (infer notTm')                        === Right b2b
  ]
