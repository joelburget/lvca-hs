{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}
module Linguist.Types where

import           Control.Lens
import           Control.Zipper
import           Data.List       (intersperse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String     (IsString(fromString))
import           Data.Text       (Text)
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Type.Reflection

-- syntax charts

newtype SyntaxChart = SyntaxChart (Map Text Sort)

data Sort = Sort
  -- !Text       -- ^ name of the sort
  ![Text]     -- ^ set of variables
  ![Operator] -- ^ set of operators

data Operator = Operator
  !Text      -- ^ operator name
  !AritySpec -- ^ arity
  !Text      -- ^ description

data Arity = Arity
  ![Valence] -- ^ the valences
  !Text      -- ^ the resulting sort

data Valence = Valence
  ![Text] -- ^ the sorts of all bound variables
  Text    -- ^ the resulting sort

-- To specify an arity, the resulting sort is unnecessary. We also include
-- variables and externals.
data AritySpec
  = Arity'        ![Valence]
  | VariableArity !Text
  | External      !Text


-- denotation charts

newtype DenotationChart a = DenotationChart (Map Text (Denotation a))

data Denotation a
  = Variable
  | Foreign !SomeTypeRep
  | CBV !([Value a] -> Value a)
  -- | Primitive !([Dynamic] -> Dynamic)
  | Substitute !Int !Int


-- judgements

data InOut = In | Out

data JudgementForm = JudgementForm
  !Text            -- ^ name of the judgement
  ![(InOut, Text)] -- ^ mode and sort of all slots

data OperatorApplication = OperatorApplication
  !Text            -- ^ operator name
  ![SaturatedTerm] -- applicands

data SaturatedTerm
  = JVariable Text
  | Op OperatorApplication

instance IsString SaturatedTerm where
  fromString = JVariable . fromString

infix 2 @@
(@@) :: Text -> [SaturatedTerm] -> SaturatedTerm
(@@) a b = Op (OperatorApplication a b)

data JudgementClause = JudgementClause
  !Text            -- ^ head (name of judgement)
  ![SaturatedTerm] -- ^ applicands

infix 2 @@@
(@@@) :: Text -> [SaturatedTerm] -> JudgementClause
(@@@) = JudgementClause

infix 2 %%%
(%%%) :: [SaturatedTerm] -> Text -> JudgementClause
(%%%) = flip JudgementClause

data JudgementRule = JudgementRule
  ![JudgementClause] -- ^ assumptions
  !JudgementClause   -- ^ conclusion

infix 0 .--
(.--) :: [JudgementClause] -> JudgementClause -> JudgementRule
(.--) = JudgementRule

newtype JudgementRules = JudgementRules [JudgementRule]

instance Pretty OperatorApplication where
  pretty (OperatorApplication hd applicands)
    = pretty hd PP.<+> hsep (pretty <$> applicands)

instance Pretty SaturatedTerm where
  pretty = \case
    JVariable name -> pretty name
    Op opAp        -> pretty opAp

instance Pretty JudgementClause where
  pretty (JudgementClause hd applicands)
    = pretty hd PP.<+> hsep (pretty <$> applicands)

instance Pretty JudgementRule where
  pretty (JudgementRule assumptions conclusion) = vsep
    [ hsep (pretty <$> assumptions)
    , "------"
    , pretty conclusion
    ]

instance Pretty JudgementRules where
  pretty (JudgementRules rules) = vsep (intersperse "" (pretty <$> rules))

instance Pretty SyntaxChart where
  pretty (SyntaxChart sorts) =
    let f (title, operators) = vsep [pretty title <> " ::=", indent 2 $ pretty operators]
    in vsep (f <$> Map.toList sorts)

instance Pretty Sort where
  pretty (Sort _vars operators)
    = vsep (pretty <$> operators)

instance Pretty Operator where
  pretty (Operator name arity _desc)
    = hsep [pretty name <> ":", pretty arity]

instance Pretty Arity where
  pretty (Arity valences result) =
    let valences' = if null valences
          then mempty
          else parens (hsep (punctuate comma (pretty <$> valences)))
    in valences' <> pretty result

instance Pretty AritySpec where
  pretty (Arity' valences) =
    if null valences
    then mempty
    else parens (hsep (punctuate comma (pretty <$> valences)))
  pretty (VariableArity name) = pretty name
  pretty (External name) = pretty name

instance Pretty Valence where
  pretty (Valence boundVars result) = hsep $
    punctuate dot (fmap pretty boundVars <> [pretty result])


-- evaluation internals

data Term a
  = Term
    !Text     -- ^ name of this term
    ![Term a] -- ^ subterms
  | Var !Text
  | PrimTerm !a

data Value a
  = NativeValue
    !Text    -- constructor(?) name
    ![Value a]
  | PrimValue !a
  deriving Show

data StackFrame a
  = CbvFrame
    !Text    -- ^ name of this term
    ![Value a] -- ^ values before
    ![Term a]  -- ^ subterms after
    !([Value a] -> Value a)  -- ^ what to do after
  | ValueBindingFrame
    !(Map Text (Value a))
  | TermBindingFrame
    !(Map Text (Term a))

findBinding :: [StackFrame a] -> Text -> Maybe (Either (Value a) (Term a))
findBinding = undefined

-- fillHole :: StackFrame -> Value -> Term
-- fillHole (CbvFrame name before after denote) tm = Term name (before ++ tm : after)

data StateStep a = StateStep
  ![StackFrame a]
  !(Either (Value a) (Term a)) -- ^ Either descending into term or ascending with value

  | Errored !Text
  | Done !(Value a)

type State = ListZipper (StateStep (Either Int Text))

type ListZipper a = Top :>> [a] :>> a

next :: State -> State
next state = case state ^. focus of
  Done{} -> state
  _      -> case state & rightward of
    Just state' -> state'
    Nothing     -> state

prev :: State -> State
prev state = case state & leftward of
  Just state' -> state'
  Nothing     -> state

type T = Either Int Text
