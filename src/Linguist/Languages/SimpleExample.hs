{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
-- patP' and termP' are unused in the generated code
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Linguist.Languages.SimpleExample
  ( statics
  , dynamics
  , typingJudgement
  , E
  , tm1
  , tm2
  , tm3
  ) where

import           Codec.Serialise
import           Control.Lens                          hiding (from, to, op)
import qualified Data.Map                              as Map
import           Data.String                           (IsString(fromString))
import           Data.Text                             (Text)
import           Data.Text.Prettyprint.Doc             (Pretty(pretty))
import           Data.Void                             (Void)
import           GHC.Generics                          (Generic)
import           NeatInterpolation
import           Text.Megaparsec

import qualified Linguist.ParseDenotationChart         as PD
import           Linguist.Types
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

$(mkTypes (Options "Exp" Nothing $ Map.fromList
  [ "Annot" :-> TupleT 0
  , "Int"   :-> ConT ''Int
  , "Text"  :-> ConT ''Text
  ])
  "Exp ::=                                                                  \n\
  \  Plus(Exp; Exp)                                                         \n\
  \  Times(Exp; Exp)                                                        \n\
  \  Cat(Exp; Exp)                                                          \n\
  \  Len(Exp)                                                               \n\
  \  Let(Exp; Exp)                                                          \n\
  \  Annotation({Annot}; Exp)                                               \n\
  \  NumLit{Int}                                                            \n\
  \  StrLit{Text}")
mkSyntaxInstances ''Exp

$(mkTypes (Options "Val" Nothing $ Map.fromList
  [ "Int"  :-> ConT ''Int
  , "Text" :-> ConT ''Text
  ])
  "Val ::=     \n\
  \  NumV{Int} \n\
  \  StrV{Text}")
mkSyntaxInstances ''Val

instance Show a => Show1 (Val a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show a, Show b) => Show (Val a b) where
  showsPrec = liftShowsPrec2 showsPrec showList showsPrec showList

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

dynamics
  :: Either (ParseErrorBundle Text Void) (DenotationChart E (Either Text E))
dynamics = runParser (PD.parseDenotationChart noParse parsePrim)
  "(arith machine dynamics)" dynamicsT

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
