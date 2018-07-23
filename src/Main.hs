{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List       (intersperse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import           Data.String     (IsString(fromString))

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

newtype SyntaxChart = SyntaxChart (Map Text Sort)

data Sort = Sort
  -- !Text       -- ^ name of the sort
  ![Text]     -- ^ set of variables
  ![Operator] -- ^ set of operators

data Operator = Operator
  !Text    -- ^ operator name
  !Arity   -- ^ arity
  !Text    -- ^ description

data Arity = Arity
  ![Valence] -- ^ the valences
  !Text      -- ^ the resulting sort

data InOut = In | Out

data JudgementForm = JudgementForm
  !Text            -- ^ name of the judgement
  ![(InOut, Text)] -- ^ mode and sort of all slots

data OperatorApplication = OperatorApplication
  !Text            -- ^ operator name
  ![SaturatedTerm] -- applicands

data SaturatedTerm
  = Variable Text
  | Op OperatorApplication

instance IsString SaturatedTerm where
  fromString = Variable . fromString

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
    = pretty hd <+> hsep (pretty <$> applicands)

instance Pretty SaturatedTerm where
  pretty = \case
    Variable name -> pretty name
    Op opAp       -> pretty opAp

instance Pretty JudgementClause where
  pretty (JudgementClause hd applicands)
    = pretty hd <+> hsep (pretty <$> applicands)

instance Pretty JudgementRule where
  pretty (JudgementRule assumptions conclusion) = vsep
    [ hsep (pretty <$> assumptions)
    , "------"
    , pretty conclusion
    ]

instance Pretty JudgementRules where
  pretty (JudgementRules rules) = vsep (intersperse "" (pretty <$> rules))

--

natJudgement :: JudgementForm
natJudgement = JudgementForm "nat" [(In, "a")]

natJudgements :: JudgementRules
natJudgements = JudgementRules
  [ []
    .--
    ["zero"] %%% "nat"
  , [["a"] %%% "nat"]
    .--
    ["succ" @@ ["a"]] %%% "nat"
  ]

--

instance Pretty SyntaxChart where
  pretty (SyntaxChart sorts) =
    let f (title, operators) = vsep [pretty title <> ":", indent 2 $ pretty operators]
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

data Valence = Valence
  ![Text] -- ^ the sorts of all bound variables
  Text    -- ^ the resulting sort

instance Pretty Valence where
  pretty (Valence boundVars result) = hsep $
    punctuate dot (fmap pretty boundVars <> [pretty result])

eChart :: SyntaxChart
eChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    -- TODO: is this the correct arity (sort)?
    [ Operator "num" (Arity [] "Typ") "numbers"
    , Operator "str" (Arity [] "Typ") "strings"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "var" (Arity [] "Exp") "variable"
    -- TODO:
    -- * do we have to really specify the resulting sort every time?
    -- * how do we refer to n, which is understood to be a number?
    , Operator "num" (Arity [Valence [] "n"] "Exp") "numeral"
    , Operator "str" (Arity [Valence [] "s"] "Exp") "literal"
    , Operator "plus" (Arity [Valence [] "Exp", Valence [] "Exp"] "Exp") "addition"
    , Operator "times" (Arity [Valence [] "Exp", Valence [] "Exp"] "Exp") "multiplication"
    , Operator "cat" (Arity [Valence [] "Exp", Valence [] "Exp"] "Exp") "concatenation"
    , Operator "len" (Arity [Valence [] "Exp"] "Exp") "length"
    -- TODO:
    -- * the book specifies this arity as
    --   - `let(e1;x.e2)`
    --   - `(Exp, Exp.Exp)Exp`
    -- * is it known that the `x` binds `e1`?
    -- * where is `x` specified?
    , Operator "let" (Arity [Valence [] "Exp", Valence ["Exp"] "Exp"] "Exp") "definition"
    ])
  ]

main :: IO ()
main = do
  putStrLn "syntax:\n"
  putDoc (pretty eChart)
  putStrLn "\n"
  putStrLn "judgements:\n"
  putDoc (pretty natJudgements)
  putStrLn "\n"
