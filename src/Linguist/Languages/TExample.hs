{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}
module Linguist.Languages.TExample where
-- Godel's system t

import           Control.Lens              (Prism, Prism', prism, prism', review, preview)
import           Data.Foldable             (asum)
import qualified Data.Map.Strict           as Map
import           Data.Void                 (Void)
import           EasyTest
import           Prelude                   hiding (succ)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty(pretty))

import           Linguist.FunctorUtil
import           Linguist.Proceed
import           Linguist.Types hiding (patP)
import qualified Linguist.Types as Types
import           Linguist.Languages.MachineModel


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

syntax :: SyntaxChart
syntax = SyntaxChart $ Map.fromList
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

-- TODO: add type annotations
data ExpF term
  = Z
  | S               !term
  | Rec !term !term !term
  | Lam             !term
  | Ap        !term !term
  deriving Functor

data ValF val
  = Zv
  | Sv !val
  deriving Functor

dynamics' :: DenotationChart' ExpF (MeaningF :+: ValF)
dynamics' = DenotationChart'
  [ FixIn Z
    :->
    Fix (InR (InL (Value (FixIn Zv))))
  , FixIn (S (Fix (InL (PatVarF (Just "e")))))
    :->
    Fix (InR (InL (Eval
      (Fix (InL (VarF "e")))
      "e'"
      (Fix (InR (InL (Value
        (Fix (InR (InR (Sv
          (Fix (InR (InL (MeaningPatternVar "e'")))))))))))))))
  -- -- TODO: rec
  , FixIn (Ap
      (FixIn (Lam (Fix (InL (PatBindingF ["x"] (Fix (InL (PatVarF (Just "body")))))))))
      (Fix (InL (PatVarF (Just "arg")))))
    :->
    Fix (InR (InL (Eval (Fix (InL (VarF "arg"))) "arg'" (Fix (InL (VarF "body"))))))
  ]

dynamics2 :: DenotationChart T (Either Text Void)
dynamics2 = DenotationChart
  [ PatternTm "Z" []
    :->
    Term "Value" [Term "Zv" []]
  , PatternTm "S" [PatternVar (Just "e")]
    :->
    Term "Eval"
      [ meaningPatternVar "e"
      , Binding ["e'"] (Term "Value" [Term "Sv" [Var "e'"]])
      ]
  , PatternTm "Ap"
    [ PatternTm "Lam" [PatternAny, BindingPattern ["x"] (PatternVar (Just "body"))]
    , PatternVar (Just "arg")
    ]
    :->
    Term "Eval"
      [ meaningPatternVar "arg"
      , Binding ["arg'"] $
        (Term "Eval"
          [ ("arg'" // "x") (meaningPatternVar "body")
          , Binding ["body'"] (Var "body'")
          ])
      ]
  ]

patP :: Prism' (Pattern T) (Fix (PatF :+: ExpF))
patP = prism' rtl ltr where
  rtl :: Fix (PatF :+: ExpF) -> Pattern T
  rtl = \case
    Fix (InL pat) -> review (Types.patP patP) pat
    Fix (InR pat) -> review patP'             pat

  ltr :: Pattern T -> Maybe (Fix (PatF :+: ExpF))
  ltr tm = asum @[]
    [ Fix . InL <$> preview (Types.patP patP) tm
    , Fix . InR <$> preview patP'             tm
    ]

  patP' :: Prism' (Pattern T) (ExpF (Fix (PatF :+: ExpF)))
  patP' = prism' rtl' ltr' where
    rtl' = \case
      Z         -> PatternTm "Z" []
      S a       -> PatternTm "S" [ review patP a ]
      Rec a b c -> PatternTm "Rec"
        [ review patP a
        , review patP b
        , review patP c
        ]
      Lam a  -> PatternTm "Lam" [ review patP a ]
      Ap a b -> PatternTm "Ap"  [ review patP a, review patP b ]

    ltr' = \case
      PatternTm "Z" []          -> Just Z
      PatternTm "S" [a]         -> S <$> preview patP a
      PatternTm "Rec" [a, b, c] -> Rec
        <$> preview patP a
        <*> preview patP b
        <*> preview patP c
      -- XXX Lam should have binding structure
      PatternTm "Lam" [a]   -> Lam <$> preview patP a
      PatternTm "Ap" [a, b] -> Ap <$> preview patP a <*> preview patP b
      _                     -> Nothing

valP :: Prism' (Term (Either Text Void)) (Fix (TermF :+: MeaningF :+: ValF))
valP = prism' rtl ltr where

  rtl :: Fix (TermF :+: MeaningF :+: ValF) -> Term (Either Text Void)
  rtl = \case
    Fix (InL tm')          -> review (termP    valP) tm'
    Fix (InR (InL tm'))    -> review (meaningP valP) tm'
    Fix (InR (InR Zv))     -> Term "Zv" []
    Fix (InR (InR (Sv v))) -> Term "Sv" [review valP v]

  ltr :: Term (Either Text Void) -> Maybe (Fix (TermF :+: MeaningF :+: ValF))
  ltr = \case
    Term "Zv" []  -> Just (Fix (InR (InR Zv)))
    Term "Sv" [t] -> Fix . InR . InR . Sv <$> preview valP t
    tm            -> asum @[]
      [ Fix . InL       <$> preview (termP    valP) tm
      , Fix . InR . InL <$> preview (meaningP valP) tm
      ]

dynamics :: DenotationChart T (Either Text Void)
dynamics = mkDenotationChart patP valP dynamics'

z, sz, ssz, pos, succ, lamapp, lamapp2 :: Term T
z = Term "Z" []
sz = Term "S" [z]
ssz = Term "S" [sz]
pos = Term "Rec" [sz, z, sz]
succ = Term "Lam" [Term "Nat" [], Binding ["x"] (Term "S" [Var "x"])]
lamapp = Term "Ap" [succ, z]
lamapp2 = Term "Ap" [succ, lamapp]

zv, szv, sszv :: Term Void
zv = Term "Zv" []
szv = Term "Sv" [zv]
sszv = Term "Sv" [szv]

eval' :: Term T -> Either String (Term Void)
eval' = eval $
  mkEvalEnv "Exp" syntax dynamics2
    (const Nothing)
    (const Nothing)

evalTests :: Test ()
evalTests = tests
  [ expect $ eval' z       == Right zv
  , expect $ eval' sz      == Right szv
  , expect $ eval' lamapp  == Right szv
  , expect $ eval' lamapp2 == Right sszv
  ]
