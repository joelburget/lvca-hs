{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
module Linguist.Languages.SimpleExample
  ( statics
  , typingJudgement
  , E
  , tm1
  , tm2
  , tm3
  ) where

import           Codec.Serialise
import           Control.Lens                          hiding (from, to, op)
import           Control.Monad.Reader
import           Data.Bifoldable
import           Data.Bitraversable
import qualified Data.Map                              as Map
import           Data.String                           (IsString(fromString))
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (Pretty(pretty))
import           Data.Void                             (Void)
import           GHC.Generics                          (Generic)
import           NeatInterpolation
import           Text.Megaparsec
import           Data.Sequence                         (Seq)

import qualified Linguist.ParseDenotationChart         as PD
import           Linguist.ParseLanguage
import           Linguist.Types
import           Linguist.Languages.MachineModel
import           Linguist.FunctorUtil
import           Linguist.ParseUtil
import           Linguist.TH

import Language.Haskell.TH.Syntax (Type(..))


newtype E = E (Either Int Text)
  deriving (Eq, Show, Generic)

instance Serialise E

makeWrapped ''E

instance Pretty E where
  pretty (E (Left  x)) = pretty x
  pretty (E (Right x)) = pretty ("\"" <> x <> "\"")

instance IsString E where
  fromString = E . Right . fromString

pattern PrimValue' :: Text -> Either Int Text -> Term E
pattern PrimValue' name x = Fix (Term name [ Fix (PrimValue (E x)) ])

pattern TI :: Int -> Term E
pattern TI x = PrimValue' "Num" (Left x)

pattern TS :: Text -> Term E
pattern TS x = PrimValue' "Str" (Right x)

$(mkTypes (Options "Exp" "syntax" $ Map.fromList
  [ ("Annot", TupleT 0)
  , ("Int", ConT ''Int)
  , ("Text", ConT ''Text)
  ])
  "Exp ::=                    \n\
  \  Plus(Exp; Exp)           \n\
  \  Times(Exp; Exp)          \n\
  \  Cat(Exp; Exp)            \n\
  \  Len(Exp)                 \n\
  \  Let(Exp; Exp)            \n\
  \  Annotation({Annot}; Exp) \n\
  \  NumLit{Int}              \n\
  \  StrLit{Text}")
mkSyntaxInstances ''Exp

-- data ExpF ty prim exp
--   = Plus       !exp !exp
--   | Times      !exp !exp
--   | Cat        !exp !exp
--   | Len        !exp
--   | Let        !exp !exp
--   | Annotation !ty  !exp
--   | NumLit     !prim
--   | StrLit     !prim
--   deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance Bitraversable (ExpF ()) where
--   bitraverse _ g (Plus       a  b) = Plus  <$> g a <*> g b
--   bitraverse _ g (Times      a  b) = Times <$> g a <*> g b
--   bitraverse _ g (Cat        a  b) = Cat   <$> g a <*> g b
--   bitraverse _ g (Len        a   ) = Len   <$> g a
--   bitraverse _ g (Let        a  b) = Let   <$> g a <*> g b
--   bitraverse _ g (Annotation () b) = Annotation () <$> g b
--   bitraverse f _ (NumLit a       ) = NumLit <$> f a
--   bitraverse f _ (StrLit a       ) = StrLit <$> f a

-- instance Bifoldable (ExpF ()) where
--   bifoldMap = bifoldMapDefault

-- instance Bifunctor (ExpF ()) where
--   bimap = bimapDefault

-- instance Bimatchable (ExpF ()) where
--   bimatchWith _ g (Plus       a1 b1) (Plus       a2 b2) = Plus  <$> g a1 a2 <*> g b1 b2
--   bimatchWith _ g (Times      a1 b1) (Times      a2 b2) = Times <$> g a1 a2 <*> g b1 b2
--   bimatchWith _ g (Cat        a1 b1) (Cat        a2 b2) = Cat   <$> g a1 a2 <*> g b1 b2
--   bimatchWith _ g (Len        a1   ) (Len        a2   ) = Len   <$> g a1 a2
--   bimatchWith _ g (Let        a1 b1) (Let        a2 b2) = Let   <$> g a1 a2 <*> g b1 b2
--   bimatchWith _ g (Annotation () b1) (Annotation () b2) = Annotation ()     <$> g b1 b2
--   bimatchWith f _ (NumLit a1   )     (NumLit a2   )     = NumLit <$> f a1 a2
--   bimatchWith f _ (StrLit a1   )     (StrLit a2   )     = StrLit <$> f a1 a2
--   bimatchWith _ _ _                  _                  = Nothing

-- instance Show ty => Show2 (ExpF ty) where
--   liftShowsPrec2 showsa _ showse _ p expf = showParen (p > 10) $ case expf of
--     Plus       a b -> ss "Plus "       . showse    11 a . ss " " . showse 11 b
--     Times      a b -> ss "Times "      . showse    11 a . ss " " . showse 11 b
--     Cat        a b -> ss "Cat "        . showse    11 a . ss " " . showse 11 b
--     Len        a   -> ss "Len "        . showse    11 a
--     Let        a b -> ss "Let "        . showse    11 a . ss " " . showse 11 b
--     Annotation t e -> ss "Annotation " . showsPrec 11 t . ss " " . showse 11 e
--     NumLit     i   -> ss "NumLit "     . showsa    11 i
--     StrLit     s   -> ss "StrLit "     . showsa    11 s
--     where ss = showString

instance Show prim => Show1 (Exp () prim) where
 liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Eq prim => Eq1 (Exp () prim) where
 liftEq = liftEq2 (==)

instance Eq ty => Eq2 (Exp ty) where
  liftEq2 _   eqe (Plus       a1 b1) (Plus       a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Times      a1 b1) (Times      a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Cat        a1 b1) (Cat        a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Len        a1   ) (Len        a2   ) = eqe a1 a2
  liftEq2 _   eqe (Let        a1 b1) (Let        a2 b2) = eqe a1 a2 && eqe b1 b2
  liftEq2 _   eqe (Annotation t1 e1) (Annotation t2 e2) = t1 == t2  && eqe e1 e2
  liftEq2 eqa  _  (NumLit     i1   ) (NumLit     i2   ) = eqa i1 i2
  liftEq2 eqa  _  (StrLit     s1   ) (StrLit     s2   ) = eqa s1 s2
  liftEq2 _   _   _                  _                  = False

data ValF prim val
  = NumV !prim
  | StrV !prim
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bitraversable ValF where
  bitraverse f _ = \case
    NumV a      -> NumV <$> f a
    StrV a      -> StrV <$> f a

instance Bifunctor ValF where
  bimap = bimapDefault

instance Bifoldable ValF where
  bifoldMap = bifoldMapDefault

instance Show a => Show1 (ValF a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Eq a => Eq1 (ValF a) where
  liftEq = liftEq2 (==)

instance Show2 ValF where
  liftShowsPrec2 showa _ _ _ p valf = showParen (p > 10) $ case valf of
    NumV i -> ss "NumV " . showa p i
    StrV s -> ss "NumV " . showa p s
    where ss = showString

instance Eq2 ValF where
  liftEq2 eqa _ (NumV i1) (NumV i2) = eqa i1 i2
  liftEq2 eqa _ (StrV s1) (StrV s2) = eqa s1 s2
  liftEq2 _   _ _         _         = False

dynamicsT :: Text
dynamicsT = [text|
  [[ Plus(n1; n2)        ]] = PrimApp({add}; [[ n1 ]]; [[ n2 ]])
  [[ Times(n1; n2)       ]] = PrimApp({mul}; [[ n1 ]]; [[ n2 ]])
  [[ Cat(n1; n2)         ]] = PrimApp({cat}; [[ n1 ]]; [[ n2 ]])
  [[ Len(e)              ]] = PrimApp({len}; [[ e ]]))
  [[ Let(e; body)        ]] = App([[ body ]]; [[ e ]])
  [[ Annotation(_; body) ]] = [[ body ]]
  [[ Num(i)              ]] = NumV([[ i ]])
  [[ Str(s)              ]] = StrV([[ s ]])
  |]

parsePrim :: PD.Parser E
parsePrim = E <$> choice
  [ Left        <$> intLiteral
  , Right "add" <$  symbol "add"
  , Right "mul" <$  symbol "mul"
  , Right "cat" <$  symbol "cat"
  , Right "len" <$  symbol "len"
  ]

dynamics'
  :: Either (ParseErrorBundle Text Void) (DenotationChart E (Either Text E))
dynamics' = runParser (PD.parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" dynamicsT

tm1F, tm2F, tm3F :: Fix (VarBindingF :+: Exp () E)

tm1F = Fix $ InR $ Annotation () $
  Fix $ InR $ Let
    (Fix $ InR $ NumLit $ E $ Left 1)
    (Fix $ InL $ BindingF ["x"] $ Fix $ InR $ Plus
      (Fix $ InL $ VarF "x")
      (Fix $ InR $ NumLit $ E $ Left 2)
      )

tm2F = Fix $ InR $ Cat
 (Fix (InR (StrLit "foo")))
 (Fix (InR (StrLit "bar")))

tm3F = Fix $ InR $ Times
  (Fix $ InR $ Len $ Fix $ InR $ StrLit "hippo")
  (Fix $ InR $ NumLit $ E $ Left 3)

tm1, tm2, tm3 :: Term E
tm1 = Fix $ Term "Annotation"
  [ TS "annotation"
  , Fix $ Term "Let"
    [ TI 1
    , Fix $ Binding ["x"] $
      Fix $ Term "Plus"
        [ Fix $ Var "x"
        , TI 2
        ]
    ]
  ]

tm2 = Fix $ Term "Cat"
  [ TS "foo"
  , TS "bar"
  ]

tm3 = Fix $ Term "Times"
  [ Fix $ Term "Len"
    [ TS "hippo"
    ]
  , TI 3
  ]

typingJudgement :: JudgementForm
typingJudgement = JudgementForm "types" [(JIn, "Exp"), (JIn, "Typ")]

statics :: JudgementRules
statics =
  let tm -: ty = [tm, ty] %%% "types"
  in JudgementRules
  [ ["x" -: "num", "y" -: "num"]
    .--
    ("plus" @@ ["x", "y"]) -: "num"
  , ["x" -: "num", "y" -: "num"]
    .--
    ("times" @@ ["x", "y"]) -: "num"
  , ["x" -: "str", "y" -: "str"]
    .--
    ("cat" @@ ["x", "y"]) -: "str"
  , ["x" -: "str"]
    .--
    ("len" @@ ["x"]) -: "num"
  -- TODO: how to do assumptions?
  -- , ["x" -: "a"]
  --   .--
  --   ("let" @@ ["x", _]) -: "a"
  ]

instance Show ((VarBindingF :+: MachineF :+: ValF (Either Text E))
          (Fix (VarBindingF :+: MachineF :+: ValF (Either Text E)))) where
  showsPrec = liftShowsPrec showsPrec showList

evalMachinePrimitiveF
  :: Text -> Maybe (Seq (ValF E (Fix (ValF E))) -> ValF E (Fix (ValF E)))
evalMachinePrimitiveF = \case
  "add" -> Just $ \case
    NumV (E (Left x))  :< NumV (E (Left y))  :< Empty -> NumV (E (Left (x + y)))
    args                      -> error $ "bad call to add: " ++ show args
  "mul" -> Just $ \case
    NumV (E (Left x))  :< NumV (E (Left y))  :< Empty -> NumV (E (Left (x * y)))
    args                      -> error $ "bad call to mul: " ++ show args
  "cat" -> Just $ \case
    StrV (E (Right x)) :< StrV (E (Right y)) :< Empty -> StrV (E (Right (x <> y)))
    args                      -> error $ "bad call to cat: " ++ show args
  "len" -> Just $ \case
    StrV (E (Right x))                       :< Empty -> NumV (E (Left (Text.length x)))
    args                      -> error $ "bad call to len: " ++ show args

  _ -> Nothing

primParsers :: ExternalParsers E
primParsers = makeExternalParsers
  [ ("Num", E . Left  <$> (intLiteral :: ExternalParser Int))
  , ("Str", E . Right <$> stringLiteral)
  ]
