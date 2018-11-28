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

import Control.Monad.State
import Data.Traversable (for)
import           Control.Lens         hiding (from, to, (??))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Diverse.Lens.Which
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import           Data.Text            (Text, unpack)
import Data.List (elem)
import           Data.Text.Prettyprint.Doc (Pretty(pretty), Doc, (<+>), parens, semi, punctuate, hsep, dot, annotate, layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Terminal

import           Linguist.Types hiding (matches)
import           Linguist.Util        ((??))

import Debug.Trace

emitTrace :: Bool
emitTrace = False

data EvalEnv a b = EvalEnv
  { _evalSort       :: !SortName
  , _evalSyntax     :: !SyntaxChart
  , _evalDenotation :: !(DenotationChart a b)
  , _evalPatternVars    :: !(Map Text (Term a))
  , _evalCorrespondences :: ![(Text, Text)]
  -- TODO: remove?
  , _evalBVars      :: !(Map Text (Term b))
  , _evalFrames     :: !Int
  , _evalPrimApp    :: !(Text -> Maybe (Seq (Term b) -> Term b))
  , _evalPrimConv   :: !(a -> Maybe b)
  }

mkEvalEnv
  :: SortName
  -> SyntaxChart
  -> DenotationChart a b
  -> (Text -> Maybe (Seq (Term b) -> Term b))
  -> (a -> Maybe b)
  -> EvalEnv a b
mkEvalEnv sort sChart dChart = EvalEnv sort sChart dChart Map.empty [] Map.empty 0

makeLenses ''EvalEnv

eval
  :: ( Eq a, Show a, Pretty a
     , Eq b, Show b, AsFacet Text b, Pretty b
     )
  => EvalEnv a b -> Term a -> Either String (Term b)
eval env tm = runReader (runExceptT (eval' tm)) env

emit :: Applicative f => Int -> String -> f ()
emit frames = when emitTrace . traceM . indent frames

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
  Var name         -> pretty name
  PrimValue name a -> pretty name <> "{" <> pretty a <> "}"

rename :: Text -> Text -> Term a -> Term a
rename from to tm = case tm of
  Term name subtms -> Term name $ fmap (rename from to) subtms
  Binding names tm' -> if from `elem` names then tm else Binding names $
    rename from to tm'
  Var name -> if name == from then Var to else tm
  PrimValue{} -> tm

fullyResolve
  :: (MonadError String m, MonadReader (EvalEnv a b) m)
  => Term b
  -> m (Term b)
fullyResolve tm = case tm of
  Term name subtms -> Term name <$> traverse fullyResolve subtms
  Binding names tm' -> Binding names <$> fullyResolve tm'
  Var name -> do
    val <- view $ evalBVars . at name
    val ?? "couldn't look up var " ++ show name
  PrimValue{} -> pure tm

getText :: AsFacet Text b => b -> Maybe Text
getText = preview (facet @Text)

eval'
  :: forall a b.
     ( Eq a, Show a, Pretty a
     , Eq b, Show b, AsFacet Text b, Pretty b
     )
  => Term a
  -> ExceptT String (Reader (EvalEnv a b)) (Term b)
eval' a = do
  frames <- view evalFrames
  emit frames "CALL eval':"
  emit frames $ "- " ++ render a
  b <- local (evalFrames %~ succ) $ eval'' a
  emit frames "RET eval':"
  emit frames $ "- " ++ render b
  pure b

eval''
  :: forall a b.
     ( Eq a, Show a, Pretty a
     , Eq b, Show b, AsFacet Text b, Pretty b
     )
  => Term a
  -> ExceptT String (Reader (EvalEnv a b)) (Term b)
eval'' (PrimValue name a) = do
  EvalEnv{_evalPrimConv=f} <- ask
  case f a of
    Nothing -> throwError "bad prim value"
    Just b  -> pure $ PrimValue name b
eval'' (Var name) = do
  val <- view $ evalBVars . at name
  val ?? "couldn't look up variable " ++ show name
eval'' tm = do
  EvalEnv sort sChart dChart _ _ _ _ _ _ <- ask
  (Subst assignments correspondences, meaning) <-
    runReaderT (findMatch dChart tm) (MatchesEnv sChart sort Map.empty)
    ?? "couldn't find match for: " ++ show tm

  frames <- view evalFrames
  emit frames $ "assignments:"
  ifor_ assignments $ \i a -> emit frames $ show i <> " -> " <> show a

  let update env = env
        -- & evalPatternVars     %~ Map.union assignments
        & evalPatternVars     .~ assignments
        & evalCorrespondences <>~ correspondences
  local update $ runInstructions meaning

  where runInstructions :: Term b -> ExceptT String (Reader (EvalEnv a b)) (Term b)
        runInstructions a = do
          frames          <- view evalFrames
          patVars         <- view evalPatternVars
          bVars           <- view evalBVars
          emit frames "CALL runInstructions:"
          emit frames $ "- " ++ render a
          ifor_ patVars $ \name val ->
            emit frames $ "+ " ++ unpack name ++ ": " ++ render val
          ifor_ bVars $ \name val ->
            emit frames $ "* " ++ unpack name ++ ": " ++ render val
          b <- local (evalFrames %~ succ) $ runInstructions' a
          emit frames "RET runInstructions:"
          emit frames $ "- " ++ render b
          pure b

        runInstructions' = \case
          -- XXX decide what the tag should be here
          Term "PrimApp" (PrimValue "Prim" val : args) -> do
            EvalEnv{_evalPrimApp=evalPrim} <- ask
            f <- evalPrim =<< getText val
              ?? "couldn't look up evaluator for this primitive"
            args' <- for args $ \case
              Var name -> do
                val' <- view $ evalBVars . at name
                val' ?? "couldn't find value for " ++ show name
              other -> pure other
            pure $ f $ Seq.fromList args'
          Term "Eval"
            [ Term "Renaming"
              [ PrimValue "Text" from
              , PrimValue "Text" to
              , patName@(Term "MeaningPatternVar" [PrimValue "Text" patternName])
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
            [ Term "MeaningPatternVar" [PrimValue "Text" fromVar]
            , Binding [name] to
            ] -> do
            fromVar' <- getText fromVar ?? "not text: " ++ show fromVar
            from     <- view $ evalPatternVars . at fromVar'
            from'    <- from ?? "couldn't look up " ++ show fromVar'
            from''   <- eval' from' -- Term a -> Term b
            from'''  <- fullyResolve from''

            local (evalBVars . at name ?~ from''') $ runInstructions to

          Term "Value" [v] -> fullyResolve v

          Var name -> do
            val  <- view $ evalBVars . at name
            val' <- val ?? "couldn't look up variable"
            pure val'

          Term "MeaningPatternVar" [PrimValue "Text" name] -> do
            throwError $ "should never evaluate a pattern var on its own ("
              ++ show name ++ ")"

          tm'@PrimValue{} -> pure tm'

          other -> throwError $ "unexpected term: " ++ show other
