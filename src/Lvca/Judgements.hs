module Lvca.Judgements
  ( SortName
  , OperatorName
  , JudgementName
  -- * Judgements
  , InOut(..)
  , JudgementForm(..)
  , OperatorApplication(..)
  , SaturatedTerm(..)
  , JudgementClause(..)
  , JudgementRule(..)
  , JudgementRules(..)
  , pattern (:@@)
  , pattern (:@@@)
  , pattern (:%%%)
  , pattern (:--)
  ) where

import           Data.List                 (intersperse)
import           Data.String               (IsString(fromString))
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding (space, (<+>))
import qualified Data.Text.Prettyprint.Doc as PP

type SortName      = Text
type OperatorName  = Text
type JudgementName = Text

data InOut = JIn | JOut

data JudgementForm = JudgementForm
  { _judgementName  :: !JudgementName       -- ^ name of the judgement
  , _judgementSlots :: ![(InOut, SortName)] -- ^ mode and sort of all slots
  }

data OperatorApplication = OperatorApplication
  { _operatorApName :: !OperatorName    -- ^ operator name
  , _applicands     :: ![SaturatedTerm] -- ^ applicands
  }

data SaturatedTerm
  = JVariable !Text
  | Op !OperatorApplication

instance IsString SaturatedTerm where
  fromString = JVariable . fromString

-- | Apply an operator to saturated terms.
infix 2 :@@
pattern (:@@) :: OperatorName -> [SaturatedTerm] -> SaturatedTerm
pattern op :@@ args = Op (OperatorApplication op args)

data JudgementClause = JudgementClause
  { _judgementHead       :: !JudgementName   -- ^ head (name of judgement)
  , _judgementApplicands :: ![SaturatedTerm] -- ^ applicands
  }

-- | Apply a judgement to its arguments
infix 2 :@@@
pattern (:@@@) :: JudgementName -> [SaturatedTerm] -> JudgementClause
pattern judg :@@@ args = JudgementClause judg args

-- | Apply a judgement to its arguments
infix 2 :%%%
pattern (:%%%) :: [SaturatedTerm] -> JudgementName -> JudgementClause
pattern args :%%% judg = JudgementClause judg args

data JudgementRule = JudgementRule
  { _assumptions :: ![JudgementClause] -- ^ assumptions
  , _conclusion  :: !JudgementClause   -- ^ conclusion
  }

-- | Separator for the premises and conclusion of a judgement
infix 0 :--
pattern (:--) :: [JudgementClause] -> JudgementClause -> JudgementRule
pattern premises :-- conclusion = JudgementRule premises conclusion

newtype JudgementRules = JudgementRules [JudgementRule]

instance Pretty OperatorApplication where
  pretty (OperatorApplication hd applicands)
    = pretty hd PP.<+> hsep (fmap pretty applicands)

instance Pretty SaturatedTerm where
  pretty = \case
    JVariable name -> pretty name
    Op opAp        -> parens $ pretty opAp

instance Pretty JudgementClause where
  pretty (JudgementClause hd applicands)
    = pretty hd PP.<+> hsep (fmap pretty applicands)

instance Pretty JudgementRule where
  pretty (JudgementRule assumptions conclusion) = vsep
    [ hsep $ punctuate comma $ fmap pretty assumptions
    , "------"
    , pretty conclusion
    ]

instance Pretty JudgementRules where
  pretty (JudgementRules rules) = vsep $ intersperse "" $ fmap pretty rules
