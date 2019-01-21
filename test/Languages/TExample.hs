{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
module Languages.TExample where
-- Godel's system t

import           Control.Lens              (Prism, Prism', prism, prism', review, preview, _Left)
import           Data.Foldable             (asum)
import qualified Data.Map.Strict           as Map
import           Data.Sequence             (Seq)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty(pretty))
import           Data.Void                 (Void)
import           Prelude                   hiding (succ)
import           NeatInterpolation         (text)

import Lvca hiding (Lam)

data T

instance Show T where show = absurd
instance Eq   T where (==) = absurd

instance Pretty T where
  pretty = absurd

absurd :: T -> a
absurd = \case

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

instance Show ((VarBindingF :+: LambdaF :+: Val (Either Text Void))
  (Fix (VarBindingF :+: (LambdaF :+: Val (Either Text Void))))) where
   showsPrec = liftShowsPrec showsPrec showList

dynamics' :: DenotationChart' (Exp Text) (LambdaF :+: Val Text)
dynamics' = DenotationChart'
  [ Fix (InR Z)
    :->
    Fix (InR (InR (InR Zv)))
  , Fix (InR (S (Fix (InL (PatVarF (Just "e"))))))
    :->
    Fix (InR (InR (InR (Sv (Fix (InR (InL (MeaningOf "e"))))))))
  -- TODO: rec
  , Fix (InR (Ap
      (Fix (InR (Lam (Fix (InL (PatVarF (Just "body")))))))
      (Fix (InL (PatVarF (Just "arg"))))))
    :->
    Fix (InR (InR (InL (App
      (Fix (InR (InL (MeaningOf "body"))))
      (Fix (InR (InL (MeaningOf "arg"))))))))
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

customPatP :: Prism' (Pattern T) (Fix (PatVarF :+: Exp a))
customPatP = prism' rtl ltr where
  rtl :: Fix (PatVarF :+: Exp a) -> Pattern T
  rtl = \case
    Fix (InL pat) -> review patVarP' pat
    Fix (InR pat) -> review patP'    pat

  ltr :: Pattern T -> Maybe (Fix (PatVarF :+: Exp a))
  ltr tm = asum @[]
    [ Fix . InL <$> preview patVarP' tm
    , Fix . InR <$> preview patP'    tm
    ]

  patP' :: Prism' (Pattern T) (Exp a (Fix (PatVarF :+: Exp a)))
  patP' = prism' rtl' ltr' where
    rtl' = \case
      Z         -> PatternTm "Z" []
      S a       -> PatternTm "S" [ review customPatP a ]
      Rec a b c -> PatternTm "Rec"
        [ review customPatP a
        , review customPatP b
        , review customPatP c
        ]
      Lam a  -> PatternTm "Lam" [ review customPatP a ]
      Ap a b -> PatternTm "Ap"  [ review customPatP a, review customPatP b ]

    ltr' = \case
      PatternTm "Z" []          -> Just Z
      PatternTm "S" [a]         -> S <$> preview customPatP a
      PatternTm "Rec" [a, b, c] -> Rec
        <$> preview customPatP a
        <*> preview customPatP b
        <*> preview customPatP c
      -- XXX Lam should have binding structure
      PatternTm "Lam" [a]   -> Lam <$> preview customPatP a
      PatternTm "Ap" [a, b] -> Ap <$> preview customPatP a <*> preview customPatP b
      _                     -> Nothing


valP :: Prism' (Term (Either Text Void))
               (Fix (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val a))
valP = prism' rtl ltr where

  rtl :: Fix (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val a)
       -> Term (Either Text Void)
  rtl = \case
    Fix (InL tm')                -> review (varBindingP valP) tm'
    Fix (InR (InL tm'))          -> review (meaningOfP textP)         tm'
    Fix (InR (InR (InL tm')))    -> review (lambdaP textP valP)  tm'
    Fix (InR (InR (InR Zv)))     -> Fix $ Term "Zv" []
    Fix (InR (InR (InR (Sv v)))) -> Fix $ Term "Sv" [review valP v]

  ltr :: Term (Either Text Void)
      -> Maybe (Fix (VarBindingF :+: MeaningOfF :+: LambdaF :+: Val a))
  ltr = \case
    Fix (Term "Zv" [])  -> Just (Fix (InR (InR (InR Zv))))
    Fix (Term "Sv" [t]) -> Fix . InR . InR . InR . Sv <$> preview valP t
    tm            -> asum @[]
      [ Fix . InL             <$> preview (varBindingP valP) tm
      , Fix . InR . InL       <$> preview (meaningOfP  textP)        tm
      , Fix . InR . InR . InL <$> preview (lambdaP textP valP)  tm
      ]

  textP :: Prism' (Term (Either Text Void)) Text
  textP = _Fix . _PrimValue . _Left

dynamics :: DenotationChart T (Either Text Void)
dynamics = mkDenotationChart customPatP valP dynamics'

z, sz, ssz, pos, succ, lamapp, lamapp2 :: Term T
z = Fix $ Term "Z" []
sz = Fix $ Term "S" [z]
ssz = Fix $ Term "S" [sz]
pos = Fix $ Term "Rec" [sz, z, sz]
succ = Fix $ Term "Lam"
  [ Fix $ Term "Nat" []
  , Fix $ Binding ["x"] $ Fix $ Term "S" [Fix $ Var "x"]
  ]
lamapp = Fix $ Term "Ap" [succ, z]
lamapp2 = Fix $ Term "Ap" [succ, lamapp]

zv, szv, sszv :: Term Void
zv = Fix $ Term "Zv" []
szv = Fix $ Term "Sv" [zv]
sszv = Fix $ Term "Sv" [szv]

-- eval' :: Term T -> Either String (Term Void)
-- eval' = eval $ mkEvalEnv "Exp" expSyntax dynamics
--     (const Nothing)
--     (const Nothing)

evalF :: Fix (VarBindingF :+: Exp Void) -> (Either String (Fix (Val Void)), Seq Text)
evalF = eval (EvalEnv Map.empty) dynamics'
