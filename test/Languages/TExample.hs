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

mkTypes (Options Nothing Map.empty)
  [text|
  Exp ::=
    Z
    S(Exp)
    Rec(Exp; Exp. Exp. Exp; Exp)
    Lam(Exp. Exp)
    Ap(Exp; Exp)
  |]
mkSyntaxInstances ''Exp

mkTypes (Options Nothing Map.empty)
  [text|
  Val ::=
    Zv
    Sv(Val)
  |]
mkSyntaxInstances ''Val

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

z, sz, ssz, pos, succ, lamapp, lamapp2 :: Term T
z = Term "Z" []
sz = Term "S" [z]
ssz = Term "S" [sz]
pos = Term "Rec" [sz, z, sz]
succ = Term "Lam"
  [ Term "Nat" []
  , Binding ["x"] $ Term "S" [Var "x"]
  ]
lamapp = Term "Ap" [succ, z]
lamapp2 = Term "Ap" [succ, lamapp]

zv, szv, sszv :: Term Void
zv = Term "Zv" []
szv = Term "Sv" [zv]
sszv = Term "Sv" [szv]

-- eval' :: Term T -> Either String (Term Void)
-- eval' = eval $ mkEvalEnv "Exp" expSyntax dynamics
--     (const Nothing)
--     (const Nothing)
