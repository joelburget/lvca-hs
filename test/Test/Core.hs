module Test.Core (coreEvalTests) where

import           EasyTest

import           Lvca.Core
import           Lvca.Types (pattern (:->))

coreEvalTests :: Test
coreEvalTests = scope "coreEval" $ tests
  [ example $ evalCore 1 === Right 1
  , example $ evalCore (Case 1 Ty [ PatternDefault :-> 0 ]) === Right 0
  , example $
    let v = evalCore $ Case 1 Ty
          [ 0              :-> 1
          , 1              :-> 0
          , PatternDefault :-> 2
          ]
    in v === Right 0
  , example $ evalCore (App (Lam ["x"] (CoreVar "x")) [1]) === Right 1
  , example $ evalCore (App (CoreVal (ValPrimop "add")) [1, 2]) === Right 3
  , example $ evalCore (App (CoreVal (ValPrimop "sub")) [1, 2])
    ===
    Right (ValLit (LitInteger (-1)))

  -- (\f x -> f x) (\x -> x) 1
  , example $ evalCore (App
    (Lam ["f", "x"] (App (CoreVar "f") [CoreVar "x"]))
      [Lam ["x"] (CoreVar "x")
      , 1
      ])
    ===
    Right 1
  ]
