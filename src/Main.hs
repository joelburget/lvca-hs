{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import Control.Lens
import           Brick           hiding ((<+>))
import qualified Brick           as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import           Data.List       (findIndex, intersperse)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import           Data.String     (IsString(fromString))
import qualified Graphics.Vty    as V
import Control.Zipper
import           Data.Text.Prettyprint.Doc hiding ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
-- import           Data.Text.Prettyprint.Doc.Render.Text

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

eChart :: SyntaxChart
eChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    -- TODO: is this the correct arity (sort)?
    [ Operator "num" (Arity' []) "numbers"
    , Operator "str" (Arity' []) "strings"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "var" (Variable "x") "variable"
    , Operator "num" (External "nat") "numeral"
    , Operator "str" (External "str") "literal"
    , Operator "plus" (Arity' [Valence [] "Exp", Valence [] "Exp"]) "addition"
    , Operator "times" (Arity' [Valence [] "Exp", Valence [] "Exp"]) "multiplication"
    , Operator "cat" (Arity' [Valence [] "Exp", Valence [] "Exp"]) "concatenation"
    , Operator "len" (Arity' [Valence [] "Exp"]) "length"
    -- TODO:
    -- * the book specifies this arity as
    --   - `let(e1;x.e2)`
    --   - `(Exp, Exp.Exp)Exp`
    -- * is it known that the `x` binds `e1`?
    -- * where is `x` specified?
    , Operator "let" (Arity' [Valence [] "Exp", Valence ["Exp"] "Exp"]) "definition"
    ])
  ]


data Term a
  = Term
    !Text     -- ^ name of this term
    ![Term a] -- ^ subterms
  | Var !Text
  | Primitive !a

eTerm1, eTerm2 :: Term (Either Int String)
eTerm1 = Term "plus"
  [ Primitive (Left 1)
  , Term "plus"
    [ Primitive (Left 2)
    , Primitive (Left 3)
    ]
  ]
eTerm2 = Term "cat" [Primitive (Right "foo"), Primitive (Right "bar")]

bordered :: Text -> Widget n -> Widget n
bordered name widget = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt name)
  $ padAll 4 widget

drawUI :: State -> Widget ()
drawUI state = case state ^. focus of
  StateStep ctx tm ->
    let lBox = bordered "Context" $ drawCtx ctx
        rBox = bordered "Term" $ drawTm tm
    in lBox B.<+> rBox
  Errored -> withAttr redAttr $ txt "error"
  Done tm -> bordered "Done" $ withAttr blueAttr $ drawTm tm

drawCtx :: EvalContext (Either Int String) -> Widget ()
drawCtx (EvalContext stack bindings) =
  vBox (fmap drawStackFrame stack)
  <=>
  hBox (fmap drawBinding (Map.toList bindings))

drawStackFrame :: HoleyTerm (Either Int String) -> Widget ()
drawStackFrame (HoleyTerm name before after) =
  let slots = padLeft (Pad 1) <$>
        (const (str "_") <$> before) ++ [str "^"] ++ (const (str "_") <$> after)
  in hBox $ txt name : slots

drawBinding :: (Text, Term (Either Int String)) -> Widget ()
drawBinding _ = txt "binding"

drawTm :: Term (Either Int String) -> Widget ()
drawTm = \case
  Term name subterms ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawTm subterms))
  Var name    -> txt name
  Primitive (Left a) -> str (show a)
  Primitive (Right a) -> str (show a)

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

app :: App State a ()
app = App
 { appDraw         = pure . drawUI
 , appChooseCursor = neverShowCursor
 , appHandleEvent  = handleEvent
 , appStartEvent   = return
 , appAttrMap      = const theMap
 }

type ListZipper a = Top :>> [a] :>> a

next :: State -> State
next state = case state & rightward of
  Just state' -> state'
  Nothing     -> state

prev :: State -> State
prev state = case state & leftward of
  Just state' -> state'
  Nothing     -> state

handleEvent :: State -> BrickEvent () a -> EventM () (Next State)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ prev g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ next g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ next g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ next g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ next g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

redAttr, blueAttr, emptyAttr :: AttrName
redAttr = "redAttr"
blueAttr = "blueAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (redAttr, B.fg V.red)
  , (blueAttr, B.fg V.blue)
  ]

isValue :: Term a -> Bool
isValue Primitive{} = True
isValue _ = False

proceed :: StateStep -> StateStep
proceed (StateStep ctx@(EvalContext stack vars) tm) = case tm of
  Term name subterms -> case findIndex (not . isValue) subterms of
    Just pos ->
      let (before, it:after) = splitAt pos subterms
          hterm = HoleyTerm name before after
      in StateStep (EvalContext (hterm:stack) vars) it
    Nothing ->
      let mResult = case tm of
            Term "plus" [Primitive (Left x), Primitive (Left y)]
              -> Just $ Primitive $ Left (x + y)
            Term "cat" [Primitive (Right x), Primitive (Right y)]
              -> Just $ Primitive $ Right (x ++ y)
            _ -> Nothing
      in case (mResult, stack) of
        (Nothing, _) -> Errored
        (Just result, []) -> Done result
        (Just result, frame:frames)
          -> StateStep (EvalContext frames vars) (fillHole frame result)
  Var name -> case vars ^. at name of
    Just tm' -> StateStep ctx tm'
    Nothing  -> Errored
  Primitive a -> StateStep ctx $ Primitive a -- complete
proceed Errored = Errored
proceed d@Done{} = d

main :: IO ()
main = do
  _ <- defaultMain app $
    let steps = iterate proceed $ StateStep (EvalContext [] (Map.empty)) eTerm1
    in zipper steps & fromWithin traverse
  pure ()

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
