{-# LANGUAGE EmptyCase       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Languages.TExample where
-- Godel's system t

import           Control.Lens
  (Prism, Prism', preview, prism, prism', review, _Left)
import           Data.Foldable             (asum)
import qualified Data.Map.Strict           as Map
import           Data.Sequence             (Seq)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty(pretty))
import           Data.Void                 (Void)
import           NeatInterpolation         (text)
import           Prelude                   hiding (succ)

import           Lvca                      hiding (Lam)

data T

instance Show T where show = absurd
instance Eq   T where (==) = absurd

instance Pretty T where
  pretty = absurd

absurd :: T -> a
absurd = \case {}

_T :: Prism s s a T
_T = prism absurd Left
{-# INLINE _T #-}

syntax' :: SyntaxChart
syntax' = SyntaxChart $ Map.fromList
  [ ("Typ", SortDef []
    [ Operator "nat" [] "naturals"
    , Operator "arr" [Valence [] "Typ", Valence [] "Typ"] "functions"
    ])
  , ("Exp", SortDef []
    [ Operator "z" [] "zero"
    , Operator "s" [Valence [] "Exp"] "successor"
    , Operator "rec"
      [ Valence [] "Exp"
      , Valence ["Exp", "Exp"] "Exp"
      , Valence [] "Exp"
      ]
      "recursion"
    , Operator "lam" [Valence [] "Typ", Valence ["Exp"] "Exp"] "abstraction"
    -- TODO: how to subscript?
    -- TODO: sugar for these definitions
    , Operator "ap" [Valence [] "Exp", Valence [] "Exp"] "application"
    ])
  ]

-- dynamics2 :: DenotationChart T (Either Text Void)
-- dynamics2 = DenotationChart
--   [ PatternTm "Z" []
--     :->
--     Term "Value" [Term "Zv" []]
--   , PatternTm "S" [PatternVar (Just "e")]
--     :->
--     Term "Sv" [ Term "Eval" [ "e" ] ]
--   , PatternTm "Ap"
--     [ PatternTm "Lam" [PatternAny, BindingPattern ["x"] (PatternVar (Just "body"))]
--     , PatternVar (Just "arg")
--     ]
--     :->
--     Term "Eval"
--       [ Eval "arg"
--       , Binding ["arg'"] $
--         (Term "Eval"
--           [ ("arg'" // "x") (Eval "body")
--           , Binding ["body'"] (Var "body'")
--           ])
--       ]
--   ]

dynamics :: DenotationChart T (Either Text Void)
dynamics = undefined -- mkDenotationChart customPatP valP dynamics'

z, sz, ssz, succ, lamapp, lamapp2 :: Term T
z = Term "Z" []
sz = Term "S" [Scope [] z]
ssz = Term "S" [Scope [] sz]
-- pos = Term "Rec" [sz, z, sz]
succ = Term "Lam"
  [ Scope []    $ Term "Nat" []
  , Scope ["x"] $ Term "S" [Scope [] $ Var "x"]
  ]
lamapp = Term "Ap" [Scope [] succ, Scope [] z]
lamapp2 = Term "Ap" [Scope [] succ, Scope [] lamapp]

zv, szv, sszv :: Term Void
zv = Term "Zv" []
szv = Term "Sv" [Scope [] zv]
sszv = Term "Sv" [Scope [] szv]

-- eval' :: Term T -> Either String (Term Void)
-- eval' = eval $ mkEvalEnv "Exp" expSyntax dynamics
--     (const Nothing)
--     (const Nothing)
