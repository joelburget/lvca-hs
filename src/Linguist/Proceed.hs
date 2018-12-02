{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Linguist.Proceed
  ( EvalEnv(..)
  , mkEvalEnv
  , eval
  , evalDenotation
  , evalPrimApp
  , evalSort
  , evalSyntax
  , evalPrimConv
  ) where

import           Control.Monad.State
import           Control.Lens              hiding (from, to, (??))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                 (elem)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text, unpack)
import           Data.Text.Prettyprint.Doc hiding (indent)
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Traversable          (for)

import           Linguist.Types hiding     (matches)
import           Linguist.Util             ((??))

import Debug.Trace

emitTrace :: Bool
emitTrace = False

data EvalEnv a b = EvalEnv
  { _evalSort            :: !SortName
  , _evalSyntax          :: !SyntaxChart
  , _evalDenotation      :: !(DenotationChart a (Either Text b))
  , _evalPatternVars     :: !(Map Text (Term a))
  , _evalCorrespondences :: ![(Text, Text)]
  , _evalVarVals         :: !(Map Text (Term b))
  , _evalFrames          :: !Int
  , _evalPrimApp         :: !(b -> Maybe (Seq (Term b) -> Term b))
  , _evalPrimConv        :: !(a -> Maybe b)
  }

makeLenses ''EvalEnv

mkEvalEnv
  :: SortName
  -> SyntaxChart
  -> DenotationChart a (Either Text b)
  -> (b -> Maybe (Seq (Term b) -> Term b))
  -> (a -> Maybe b)
  -> EvalEnv a b
mkEvalEnv sort sChart dChart
  = EvalEnv sort sChart dChart Map.empty [] Map.empty 0

eval
  :: ( Eq a, Show a, Pretty a
     , Eq b, Show b, Pretty b
     )
  => EvalEnv a b -> Term a -> Either String (Term b)
eval env tm = runReader (runExceptT (eval' tm)) env

emit :: MonadReader (EvalEnv a b) m => String -> m ()
emit msg = do
  frames <- view evalFrames
  when emitTrace $ traceM $ indent frames msg

emitD :: MonadReader (EvalEnv a b) m => Doc AnsiStyle -> m ()
emitD = emit . unpack . renderStrict . layoutPretty defaultLayoutOptions

render :: Pretty a => Term a -> String
render
  = unpack
  . renderStrict
  . layoutPretty defaultLayoutOptions
  . specialPretty

indent :: Int -> String -> String
indent i =
  let level = min (i * 2) 20
  in (replicate level ' ' ++)

specialPretty :: Pretty a => Term a -> Doc AnsiStyle
specialPretty = \case
  Term name subtms ->
    let nameColor = case name of
          "PrimApp"           -> color Green
          "Eval"              -> color Green
          "Renaming"          -> color Green
          "Value"             -> color Green
          "MeaningPatternVar" -> color Green
          _                   -> mempty
    in annotate nameColor (pretty name) <>
         parens (hsep $ punctuate semi $ fmap specialPretty subtms)
  Binding names tm ->
    hsep (punctuate dot (fmap pretty names)) <> dot <+> specialPretty tm
  Var name    -> pretty name
  PrimValue a -> annotate (color Blue) $ "{" <> pretty a <> "}"

rename :: Text -> Text -> Term a -> Term a
rename from to tm = case tm of
  Term name subtms -> Term name $ fmap (rename from to) subtms
  Binding names tm' -> if from `elem` names then tm else Binding names $
    rename from to tm'
  Var name -> if name == from then Var to else tm
  PrimValue{} -> tm

erase :: Term (Either Text b) -> ExceptT String (Reader (EvalEnv a b)) (Term b)
erase = traverse $ \case
  Left text -> throwError $ "when erasing, found pattern var " ++ unpack text
  Right b   -> pure b

fullyResolve
  :: (MonadError String m, MonadReader (EvalEnv a b) m, Pretty b, Show b, Show a)
  => Term b
  -> m (Term b)
fullyResolve tm = case tm of
  Term name subtms  -> Term name <$> traverse fullyResolve subtms
  Binding names tm' -> Binding names <$> fullyResolve tm'
  Var name -> do
    val     <- view $ evalVarVals . at name
    varVals <- view evalVarVals
    patVars <- view evalPatternVars
    case val of
      Just val' -> do
        emit $ show name
        emit $ render val'
      Nothing -> do
        emit $ show varVals
        emit $ show patVars
    val ?? "couldn't look up var " ++ show name
  PrimValue{} -> pure tm

getText :: Either Text b -> Maybe Text
getText = \case
  Left text -> Just text
  Right _   -> Nothing

eval'
  :: forall a b.
     ( Eq a, Show a, Pretty a
     , Eq b, Show b, Pretty b
     )
  => Term a
  -> ExceptT String (Reader (EvalEnv a b)) (Term b)
eval' a = do
  emit "CALL eval':"
  emit $ "- " ++ render a
  b <- local (evalFrames %~ succ) $ eval'' a
  emit "RET eval':"
  emit $ "- " ++ render b
  pure b

newtype PrettyEither b = PrettyEither (Either Text b)

instance Pretty b => Pretty (PrettyEither b) where
  pretty (PrettyEither (Left  x)) = "\"" <> pretty x <> "\""
  pretty (PrettyEither (Right x)) = pretty x

eval''
  :: forall a b.
     ( Eq a, Show a, Pretty a
     , Eq b, Show b, Pretty b
     )
  => Term a
  -> ExceptT String (Reader (EvalEnv a b)) (Term b)
-- TODO: are these first two cases kosher? wrong domain to be analyzed here?
eval'' (PrimValue a) = do
  -- testing whether these cases are hit
  emitD $ annotate (color Red) "PRIMVALUE eval"
  EvalEnv{_evalPrimConv=f} <- ask
  case f a of
    Nothing -> throwError "bad prim value"
    Just b  -> pure $ PrimValue b
eval'' (Var name) = do
  -- testing whether these cases are hit
  emitD $ annotate (color Red) "VAR eval"
  val <- view $ evalVarVals . at name
  val ?? "couldn't look up variable " ++ show name
eval'' tm = do
  EvalEnv sort sChart dChart _ _ _ _ _ _ <- ask
  (Subst patternAssignments correspondences, meaning) <-
    runReaderT (findMatch dChart tm) (MatchesEnv sChart sort Map.empty)
    ?? "couldn't find match for: " ++ show tm

  showAll "pattern assignments" patternAssignments

  let update env = env
        & evalPatternVars     .~ patternAssignments
        & evalCorrespondences <>~ correspondences
  local update $ runInstructions meaning

  where runInstructions :: Term (Either Text b) -> ExceptT String (Reader (EvalEnv a b)) (Term b)
        runInstructions a = do
          patVars <- view evalPatternVars
          varVals <- view evalVarVals
          emit "CALL runInstructions:"
          emit $ "- " ++ render (PrettyEither <$> a)
          showAll "pattern vars" patVars
          showAll "variables" varVals
          b <- local (evalFrames %~ succ) $ runInstructions' a
          emit "RET runInstructions:"
          emit $ "- " ++ render b
          pure b

        showAll label vals =
          if null vals
            then emit $ "(no " <> label <> ")"
            else do emit $ label <> ":"
                    ifor_ vals $ \name val ->
                      emit $ "* " ++ unpack name ++ " -> " ++ render val

        runInstructions'
          :: Term (Either Text b)
          -> ExceptT String (Reader (EvalEnv a b)) (Term b)
        runInstructions' = \case

          Term "PrimApp" (PrimValue val : args) -> do
            EvalEnv{_evalPrimApp=evalPrim} <- ask
            emit $ "val: " ++ show val
            val' <- case val of
              Left _  -> throwError "invariant violation: found a pattern \
                \variable where a primitive value was expected"
              Right x -> pure x
            f <- evalPrim val'
              ?? "couldn't look up evaluator for this primitive"
            args' <- for args $ \case
              Var name -> do
                val'' <- view $ evalVarVals . at name
                val'' ?? "couldn't find value for " ++ show name
              other -> erase other
            pure $ f $ Seq.fromList args'

          Term "Eval"
            [ Term "Renaming"
              [ PrimValue from
              , PrimValue to
              , patName@(Term "MeaningPatternVar" [PrimValue patternName])
              ]
            , rhs
            ] -> do
            from'        <- getText from        ?? "not text: " ++ show from
            to'          <- getText to          ?? "not text: " ++ show to
            patternName' <- getText patternName ?? "not text: " ++ show patternName
            tm'          <- view $ evalPatternVars . at patternName'
            tm''         <- tm' ?? "couldn't look up term to rename"
            let tm''' = rename from' to' tm''
            local (evalPatternVars . at patternName' ?~ tm''') $
              runInstructions $ Term "Eval" [patName, rhs]

          Term "Eval"
            [ Term "MeaningPatternVar" [PrimValue fromVar]
            , Binding [name] to
            ] -> do
            fromVar' <- getText fromVar ?? "not text: " ++ show fromVar
            from     <- view $ evalPatternVars . at fromVar'
            from'    <- from ?? "couldn't look up " ++ show fromVar'
            from''   <- eval' from' -- Term a -> Term b
            from'''  <- fullyResolve from''

            local (evalVarVals . at name ?~ from''') $ runInstructions to

          Term "Value" [v] -> do
            emit "here: Term Value"
            emit $ render $ PrettyEither <$> v
            fullyResolve =<< erase v

          Var name -> do
            val  <- view $ evalVarVals . at name
            val' <- val ?? "couldn't look up variable"
            pure val'

          Term "MeaningPatternVar" [PrimValue name] -> do
            throwError $ "should never evaluate a pattern var on its own ("
              ++ show name ++ ")"

          tm'@PrimValue{} -> erase tm'

          other -> do
            emit $ "unexpected term: " ++ render (PrettyEither <$> other)
            throwError "unexpected term"
