{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}
module Linguist.Languages.TExample where
-- Godel's system t

import           Control.Lens              (Prism, Prism', prism, prism', review, preview)
import           Data.Foldable             (asum)
import qualified Data.Map.Strict           as Map
import           Data.Sequence             (Seq)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc (Pretty(pretty))
import           Data.Void                 (Void)
import           EasyTest
import           Prelude                   hiding (succ)

import           Linguist.FunctorUtil
import           Linguist.Proceed2         (eval, EvalEnv(EvalEnv))
import           Linguist.Types
import qualified Linguist.Languages.MachineModel as M

import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable


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
data ExpF prim term
  = Z
  | S               !term
  | Rec !term !term !term
  | Lam             !term
  | Ap        !term !term
  deriving Functor

instance Bifoldable ExpF where
instance Bifunctor ExpF where
instance Bitraversable ExpF where
instance Bimatchable ExpF where

instance Bifoldable ValF where
instance Bifunctor ValF where
instance Bitraversable ValF where
instance Traversable (ValF (Either Text a)) where
instance Foldable (ValF (Either Text a)) where

instance Show ((:+:) VarBindingF (M.MachineF :+: ValF (Either Text Void))
                           (Fix (VarBindingF :+: (M.MachineF :+: ValF (Either Text Void)))))

instance Show1 (ValF Text)
instance Show1 (ValF Void)
instance Show1 (ExpF Void)
instance Show1 (ValF (Either Text Void))

data ValF prim val
  = Zv
  | Sv !val
  deriving Functor

dynamics' :: DenotationChart' (ExpF Text) (M.MachineF :+: ValF Text)
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
    Fix (InR (InR (InL (M.App
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

patP :: Prism' (Pattern T) (Fix (PatVarF :+: ExpF a))
patP = prism' rtl ltr where
  rtl :: Fix (PatVarF :+: ExpF a) -> Pattern T
  rtl = \case
    Fix (InL pat) -> review patVarP pat
    Fix (InR pat) -> review patP'   pat

  ltr :: Pattern T -> Maybe (Fix (PatVarF :+: ExpF a))
  ltr tm = asum @[]
    [ Fix . InL <$> preview patVarP tm
    , Fix . InR <$> preview patP'   tm
    ]

  patP' :: Prism' (Pattern T) (ExpF a (Fix (PatVarF :+: ExpF a)))
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

valP :: Prism' (Term (Either Text Void)) (Fix (VarBindingF :+: MeaningOfF :+: M.MachineF :+: ValF a))
valP = prism' rtl ltr where

  rtl :: Fix (VarBindingF :+: MeaningOfF :+: M.MachineF :+: ValF a) -> Term (Either Text Void)
  rtl = \case
    Fix (InL tm')                -> review (varBindingP valP) tm'
    Fix (InR (InL tm'))          -> review meaningP           tm'
    Fix (InR (InR (InL tm')))    -> review (M.machineP valP)  tm'
    Fix (InR (InR (InR Zv)))     -> Fix $ Term "Zv" []
    Fix (InR (InR (InR (Sv v)))) -> Fix $ Term "Sv" [review valP v]

  ltr :: Term (Either Text Void) -> Maybe (Fix (VarBindingF :+: MeaningOfF :+: M.MachineF :+: ValF a))
  ltr = \case
    Fix (Term "Zv" [])  -> Just (Fix (InR (InR (InR Zv))))
    Fix (Term "Sv" [t]) -> Fix . InR . InR . InR . Sv <$> preview valP t
    tm            -> asum @[]
      [ Fix . InL             <$> preview (varBindingP valP) tm
      , Fix . InR . InL       <$> preview meaningP           tm
      , Fix . InR . InR . InL <$> preview (M.machineP valP)  tm
      ]

dynamics :: DenotationChart T (Either Text Void)
dynamics = M.mkDenotationChart patP valP dynamics'

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
-- eval' = eval $ mkEvalEnv "Exp" syntax dynamics
--     (const Nothing)
--     (const Nothing)

evalF :: Fix (VarBindingF :+: ExpF Void) -> (Either String (Fix (ValF Void)), Seq Text)
evalF = eval (EvalEnv Map.empty (const Nothing)) dynamics'

-- evalTests :: Test ()
-- evalTests = tests
--   [ expect $ eval' z       == Right zv
--   , expect $ eval' sz      == Right szv
--   , expect $ eval' lamapp  == Right szv
--   , expect $ eval' lamapp2 == Right sszv
--   ]
