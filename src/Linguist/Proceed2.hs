{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Linguist.Proceed2 where

import           Control.Lens              hiding (from, to, (??))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               (First (First, getFirst))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import           Data.Foldable             (toList)

import           Linguist.Languages.MachineModel
import           Linguist.Types hiding     (matches)
import           Linguist.Util             ((??), (???))
import           Linguist.FunctorUtil

import Debug.Trace

data EvalEnv f = EvalEnv
  { _evalVarVals         :: !(Map Text (Fix f))
  -- , _evalFrames          :: !Int
  , _evalPrimApp         :: !(Text -> Maybe (Seq (f (Fix f)) -> f (Fix f)))
  }

makeLenses ''EvalEnv

newtype EvalM f a = EvalM { runEvalM :: ExceptT String (Reader (EvalEnv f)) a }
  deriving (Functor, Applicative, Monad, MonadReader (EvalEnv f),
    MonadError String)

eval
  :: (Show1 f, Eq1 f, Zippable f, Foldable f, Traversable g, Show1 g)
  => EvalEnv g
  -> DenotationChart' f (MachineF :+: g)
  -> Fix (TermF :+: f)
  -> Either String (Fix g)
eval env chart tm = case translate chart tm of
  Nothing -> Left "failed to translate term"
  Just tm' -> runReader (runExceptT (runEvalM (eval' tm'))) env

findMatch'
  :: forall f g. (Show1 g, Show1 f, Eq1 f, Zippable f, Foldable f)
  => DenotationChart' f (MachineF :+: g)
  -> Fix (TermF :+: f)
  -> Maybe ( Map Text (Fix (TermF :+: f))
           , Fix (TermF :+: MeaningOfF :+: MachineF :+: g)
           )
findMatch' (DenotationChart' cases) tm = getFirst $ foldMap
  (\(pat, rhs) -> First $ (,rhs) <$> matches pat tm)
  cases

matches
  :: (Zippable f, Foldable f)
  => Fix (PatF :+: f)
  -> Fix (TermF :+: f)
  -> Maybe (Map Text (Fix (TermF :+: f)))
matches (Fix pat) (Fix tm) = case pat of
  InL PatBindingF{}  -> Nothing -- TODO
  InL (PatVarF Nothing)     -> Just $ Map.empty
  InL (PatVarF (Just name)) -> Just $ Map.singleton name $ Fix tm
  InR pat'           -> case tm of
    InL tmtm -> case tmtm of
      BindingF{} -> Nothing -- TODO
      VarF{}     -> Nothing -- TODO
    InR tm' -> matches' pat' tm'

matches'
  :: (Zippable f, Foldable f)
  => f (Fix (PatF :+: f))
  -> f (Fix (TermF :+: f))
  -> Maybe (Map Text (Fix (TermF :+: f)))
matches' f1 f2 = Map.unions . toList <$> fzip matches f1 f2

translate
  :: (Show1 g, Show1 f, Eq1 f, Zippable f, Foldable f, Traversable g)
  => DenotationChart' f (MachineF :+: g)
  -> Fix (TermF :+: f)
  -> Maybe (Fix (TermF :+: MachineF :+: g))
translate chart tm = case unfix tm of
  InL (BindingF names subtm)
    -> Fix . InL . BindingF names <$> translate chart subtm
  InL (VarF name) -> Just $ Fix $ InL $ VarF name
  InR _ -> do
    traceM $ "finding match for " ++ show tm
    (varBindings, rhs) <- findMatch' chart tm
    traceM $ "varBindings: " ++ show varBindings
    traceM $ "rhs: " ++ show rhs
    runReaderT (translate' rhs) $ TranslateEnv chart varBindings

data TranslateEnv f g = TranslateEnv
  { _dChart :: DenotationChart' f (MachineF :+: g)
  , _varBindings :: Map Text (Fix (TermF :+: f))
  }

translate'
  :: (Show1 g, Show1 f, Eq1 f, Zippable f, Foldable f, Traversable g)
  => Fix (TermF :+: MeaningOfF :+: MachineF :+: g)
  -> ReaderT (TranslateEnv f g) Maybe (Fix (TermF :+: MachineF :+: g))
translate' (Fix tm) = case tm of
  InR (InL (MeaningOf name)) -> do
    traceM $ "translate' MeaningOf " ++ show name
    TranslateEnv chart varBindings <- ask
    traceM $ "val " ++ show (Map.lookup name varBindings)
    lift $ translate chart =<< Map.lookup name varBindings
  InL tm'             -> Fix . InL       <$> traverse translate' tm'
  InR (InR (InL tm')) -> Fix . InR . InL <$> traverse translate' tm'
  InR (InR (InR tm')) -> Fix . InR . InR <$> traverse translate' tm'

purify :: Traversable f
  => Fix (TermF :+: MachineF :+: f) -> EvalM f (Fix f)
purify (Fix tm) = case tm of
  InL _         -> throwError "found TermF in purification"
  InR (InL _)   -> throwError "found MachineF in purification"
  InR (InR tm') -> Fix <$> traverse purify tm'

-- | substitute @arg@ for @name@ in @body@
subst
  :: Traversable f
  => Text
  -> Fix (TermF :+: MachineF :+: f)
  -> Fix (TermF :+: MachineF :+: f)
  -> EvalM f (Fix (TermF :+: MachineF :+: f))
subst name arg (Fix body) = case body of
  InL (VarF name')
    -> pure $ if name == name' then arg else Fix $ InL $ VarF name'
  InL (BindingF names body')
    -> Fix . InL . BindingF names <$> subst name arg body'
  InR f
    -> Fix . InR <$> traverse (subst name arg) f

eval' :: Traversable f => Fix (TermF :+: MachineF :+: f) -> EvalM f (Fix f)
eval' (Fix f) = case f of
  InL BindingF{}  -> throwError "bare binding"
  InL (VarF name) -> view (evalVarVals . at name)
    ??? "couldn't look up variable " ++ show name
  InR (InL (App (Fix (InR (InL (Lam name body)))) arg)) -> do
    ret <- subst name arg body
    purify ret
  InR (InL App{}) -> throwError "invalid app"
  InR (InL Lam{}) -> throwError "bare lambda"
  InR (InL (PrimApp name args)) -> do
    primApps <- view evalPrimApp
    fun      <- primApps name ?? "couldn't look up prim function " ++ show name
    args'    <- traverse purify args
    pure $ Fix $ fun $ Seq.fromList $ unfix <$> args'
  InR (InR tm) -> Fix <$> traverse eval' tm
