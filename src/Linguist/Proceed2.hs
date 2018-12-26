{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Linguist.Proceed2 where

import           Control.Lens              hiding (from, to, (??))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               (First (First, getFirst))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Linguist.Languages.MachineModel
import           Linguist.Types hiding     (matches)
import           Linguist.Util             ((??), (???))
import           Linguist.FunctorUtil

import Control.Monad.Writer.CPS
import Control.Monad.Trans.Maybe

import Debug.Trace

data EvalEnv f = EvalEnv
  { _evalVarVals         :: !(Map Text (Fix f))
  -- , _evalFrames          :: !Int
  , _evalPrimApp         :: !(Text -> Maybe (Seq (f (Fix f)) -> f (Fix f)))
  }

makeLenses ''EvalEnv

newtype EvalM f a = EvalM {
  runEvalM :: ExceptT String (WriterT (Seq Text) (Reader (EvalEnv f))) a
  } deriving (Functor, Applicative, Monad, MonadReader (EvalEnv f),
    MonadError String, MonadWriter (Seq Text))

type DomainFunctor f a =
  ( Bifoldable f
  , Eq2 f
  , Show2 f
  , Zippable f
  , Show1 (f a)
  , Eq1 (f Text)
  , Foldable (f Text)
  , Foldable (f (Text, a))
  , Show ((:+:) PatF (f Text) (Fix (PatF :+: f Text)))
  , Show ((:+:) TermF (f a) (Fix (TermF :+: f a)))
  , Show (f (Text, a) (Map Text a, Map Text (Fix (TermF :+: f a))))
  )

type CodomainFunctor g a =
  ( Show2 g
  , Bitraversable g
  , Traversable (g Text)
  , Show1 (g Text)
  , Traversable (g a)
  , Traversable (g (Either Text a))
  )

eval
  :: (DomainFunctor f a, CodomainFunctor g a, Show a)
  => EvalEnv (g a)
  -> DenotationChart' (f Text) (MachineF :+: g Text)
  -> Fix (TermF :+: f a)
  -> (Either String (Fix (g a)), Seq Text)
eval env chart tm = case runWriter (runMaybeT (translate chart tm)) of
  (Nothing, logs) -> (Left "failed to translate term", logs)
  (Just tm', logs) ->
    let (val, logs') = runReader (runWriterT (runExceptT (runEvalM (eval' tm')))) env
    in (val, logs <> logs')

findMatch'
  :: forall f g a.
     (DomainFunctor f a, CodomainFunctor g a , Show a)
  => DenotationChart' (f Text) (MachineF :+: g Text)
  -> Fix (TermF :+: f a)
  -> Maybe ( (Map Text a, Map Text (Fix (TermF :+: f a)))
           , Fix (TermF :+: MeaningOfF :+: MachineF :+: g Text)
           )
findMatch' (DenotationChart' cases) tm = getFirst $ foldMap
  (\(pat, rhs) -> First $ (,rhs) <$> matches pat tm)
  cases

matches
  :: (DomainFunctor f a, Show a)
  => Fix (PatF  :+: f Text)
  -> Fix (TermF :+: f a   )
  -> Maybe (Map Text a, Map Text (Fix (TermF :+: f a)))
matches (Fix pat) (Fix tm) = do
  -- traceM $ "matches considering " ++ show pat ++ " / " ++ show tm
  case pat of
    InL PatBindingF{}         -> Nothing -- TODO
    InL (PatVarF Nothing)     -> Just (Map.empty, Map.empty)
    InL (PatVarF (Just name)) -> Just (Map.empty, Map.singleton name $ Fix tm)
    InR pat'           -> case tm of
      InL tmtm -> case tmtm of
        BindingF{} -> Nothing -- TODO
        VarF{}     -> Nothing -- TODO
      InR tm' -> matches' pat' tm'

matches'
  :: (DomainFunctor f a, Show a)
  => f Text (Fix (PatF  :+: f Text))
  -> f a    (Fix (TermF :+: f a   ))
  -> Maybe (Map Text a, Map Text (Fix (TermF :+: f a)))
matches' f1 f2 = do
  zipped <- fzip (fmap Just . (,)) matches f1 f2
  traceM $ "zipped: " ++ show zipped
  let allMatches@(primVarMatches, varMatches) = bifoldr
        (\(i, a) (m1, m2) -> (Map.insert i a m1, m2))
        (<>)
        (Map.empty, Map.empty)
        zipped
  traceM $ "primVarMatches: " ++ show primVarMatches
  traceM $ "varMatches: " ++ show varMatches
  pure $ allMatches

translate
  :: forall f g a m.
     ( DomainFunctor f a, CodomainFunctor g a
     , MonadWriter (Seq Text) m
     , Show a
     )
  => DenotationChart' (f Text) (MachineF :+: g Text)
  -> Fix (TermF :+: f a)
  -> MaybeT m (Fix (TermF :+: MachineF :+: g (Either Text a)))
translate chart tm = case unfix tm of
  InL (BindingF names subtm)
    -> Fix . InL . BindingF names <$> translate chart subtm
  InL (VarF name) -> pure $ Fix $ InL $ VarF name
  InR _ -> do
    tell1 $ "finding match for " <> tShow tm
    ((primVarBindings, varBindings), rhs) <- MaybeT $ pure $ findMatch' chart tm
    tell1 $ "varBindings: " <> tShow varBindings
    tell1 $ "primVarBindings: " <> tShow primVarBindings
    tell1 $ "rhs: " <> tShow rhs
    mapMaybeT (`runReaderT` TranslateEnv chart primVarBindings varBindings)
      (translate' rhs)

tShow :: Show a => a -> Text
tShow = Text.pack . show

tell1 :: MonadWriter (Seq Text) m => Text -> m ()
tell1 = tell . Seq.singleton

data TranslateEnv f g a = TranslateEnv
  { _dChart :: DenotationChart' (f Text) (MachineF :+: g Text)
  , _primVarBindings :: Map Text a
  , _varBindings :: Map Text (Fix (TermF :+: f a))
  }

translate'
  :: ( DomainFunctor f a, CodomainFunctor g a
     , MonadWriter (Seq Text) m, MonadReader (TranslateEnv f g a) m
     , Show a
     )
  => Fix (TermF :+: MeaningOfF :+: MachineF :+: g Text)
  -> MaybeT m (Fix (TermF :+: MachineF :+: g (Either Text a)))
translate' (Fix tm) = case tm of
  InR (InL (MeaningOf name)) -> do
    tell1 $ "translate' MeaningOf " <> tShow name
    TranslateEnv chart primVarBindings varBindings <- ask
    tell1 $ "prim val " <> tShow (Map.lookup name primVarBindings)
    tell1 $ "val " <> tShow (Map.lookup name varBindings)
    binding <- MaybeT $ pure $ Map.lookup name varBindings
    translate chart binding
  InL tm'             -> Fix . InL       <$> traverse translate' tm'
  InR (InR (InL tm')) -> Fix . InR . InL <$> traverse translate' tm'
  InR (InR (InR tm')) -> Fix . InR . InR <$> bitraverse
    (\name -> do
      TranslateEnv _ primVarBindings _ <- ask
      case Map.lookup name primVarBindings of
        Nothing -> MaybeT $ pure Nothing
        Just val -> pure $ Right val)
    -- (pure . Left)
    translate'
    tm'

purify
  :: Bitraversable f
  => Fix (TermF :+: MachineF :+: f (Either Text a)) -> EvalM (f a) (Fix (f a))
purify (Fix tm) = case tm of
  InL _         -> throwError "found TermF in purification"
  InR (InL _)   -> throwError "found MachineF in purification"
  InR (InR tm') -> Fix <$> bitraverse expectRight purify tm'

expectRight :: Show a => Either a b -> EvalM f b
expectRight = \case
  Left name -> throwError $
    "found (prim) pattern variable in purification " ++ show name
  Right a -> pure a

-- | substitute @arg@ for @name@ in @body@
subst
  :: Traversable f
  => Text
  -> Fix (TermF :+: MachineF :+: f)
  -> Fix (TermF :+: MachineF :+: f)
  -> EvalM f' (Fix (TermF :+: MachineF :+: f))
subst name arg (Fix body) = case body of
  InL (VarF name')
    -> pure $ if name == name' then arg else Fix $ InL $ VarF name'
  InL (BindingF names body')
    -> Fix . InL . BindingF names <$> subst name arg body'
  InR f
    -> Fix . InR <$> traverse (subst name arg) f

eval'
  :: (Bitraversable f, Traversable (f (Either Text a)))
  => Fix (TermF :+: MachineF :+: f (Either Text a))
  -> EvalM (f a) (Fix (f a))
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
  InR (InR tm) -> Fix <$> bitraverse expectRight eval' tm
