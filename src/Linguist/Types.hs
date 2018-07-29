{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
  = Arity'   ![Valence]
  | Variable !Text
  | External !Text

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
  pretty (Variable name) = pretty name
  pretty (External name) = pretty name

instance Pretty Valence where
  pretty (Valence boundVars result) = hsep $
    punctuate dot (fmap pretty boundVars <> [pretty result])

data Term a
  = Term
    !Text     -- ^ name of this term
    ![Term a] -- ^ subterms
  | Var !Text
  | Primitive !a

data HoleyTerm a = HoleyTerm
  !Text     -- ^ name of this term
  ![Term a] -- ^ subterms before
  ![Term a] -- ^ subterms after

fillHole :: HoleyTerm a -> Term a -> Term a
fillHole (HoleyTerm name before after) tm = Term name (before ++ tm : after)

data EvalContext a = EvalContext
  ![HoleyTerm a]
  -- This doesn't really work -- we have no way to clear bindings! We need to
  -- associate bindings with a stack frame.
  !(Map Text (Term a))

data StateStep = StateStep
  !(EvalContext (Either Int String))
  !(Term (Either Int String))

  | Errored
  | Done (Term (Either Int String))

type State = ListZipper StateStep

type ListZipper a = Top :>> [a] :>> a

next :: State -> State
next state = case state & rightward of
  Just state' -> state'
  Nothing     -> state

prev :: State -> State
prev state = case state & leftward of
  Just state' -> state'
  Nothing     -> state
