{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE TemplateHaskell     #-}
module Lvca.Proceed where

import           Control.Lens              hiding (from, to, (??))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer.CPS
import           Data.Bifoldable
import           Data.Bitraversable
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               (First (First, getFirst))
import           Data.Sequence             (Seq)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Lvca.Languages.MachineModel
import           Lvca.Types            hiding (findMatch, matches)
import           Lvca.Util             ((??), (???))
import           Lvca.FunctorUtil

data TranslateEnv f g a = TranslateEnv
  { _dChart          :: DenotationChart' (f Text) (MachineF :+: g Text)
  , _primVarBindings :: Map Text a
  , _varBindings     :: Map Text (Fix (VarBindingF :+: f a))
  }

data EvalEnv f = EvalEnv
  { _evalVarVals         :: !(Map Text (Fix f))
  -- , _evalFrames          :: !Int
  , _evalPrimApp         :: !(Text -> Maybe (Seq (f (Fix f)) -> f (Fix f)))
  }

makeLenses ''EvalEnv

data MatchResult f a = MatchResult
  { primVarMatches :: Map Text a
  , varMatches     :: Map Text (Fix (VarBindingF :+: f a))
  } deriving Show

instance Semigroup (MatchResult f a) where
  MatchResult a1 b1 <> MatchResult a2 b2 = MatchResult (a1 <> a2) (b1 <> b2)

instance Monoid (MatchResult f a) where
  mempty = MatchResult Map.empty Map.empty

newtype EvalM f a = EvalM {
  runEvalM :: ExceptT String (WriterT (Seq Text) (Reader (EvalEnv f))) a
  } deriving (Functor, Applicative, Monad, MonadReader (EvalEnv f),
    MonadError String, MonadWriter (Seq Text))

type DomainFunctor f a =
  ( Bifoldable f
  , Bimatchable f
  , Show1 (f a)
  )

type CodomainFunctor g a =
  ( Bitraversable g
  -- TODO: This could be subsumed by Show2
  , Show1 (g Text)
  , Show1 (g (Either Text a))
  -- TODO: this should be subsumed by the Bitraversable constraint:
  , Traversable (g (Either Text a))
  )

eval
  :: forall f g a.
     (DomainFunctor f a, CodomainFunctor g a, Show a)
  => EvalEnv (g a)
  -> DenotationChart' (f Text) (MachineF :+: g Text)
  -> Fix (VarBindingF :+: f a)
  -> (Either String (Fix (g a)), Seq Text)
eval env chart tm = case runWriter (runMaybeT (translate chart tm)) of
  (Nothing, logs) -> (Left "failed to translate term", logs)
  (Just tm', logs) ->
    let (val, logs') = runReader (runWriterT (runExceptT (runEvalM (eval' tm')))) env
    in (val, logs <> logs')

findMatch
  :: forall f g a.
     (DomainFunctor f a, CodomainFunctor g a , Show a)
  => DenotationChart' (f Text) (MachineF :+: g Text)
  -> Fix (VarBindingF :+: f a)
  -> Maybe ( MatchResult f a
           , Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: g Text)
           )
findMatch (DenotationChart' cases) tm = getFirst $ foldMap
  (\(pat, rhs) -> First $ (,rhs) <$> matches pat tm)
  cases

-- TODO: I think we could automate all this via Bimatchable instances
-- | Whether this functor matches the pattern.
matches
  :: (DomainFunctor f a, Show a)
  => Fix (PatVarF     :+: f Text)
  -> Fix (VarBindingF :+: f a   )
  -> Maybe (MatchResult f a)
matches (Fix pat) (Fix tm) = case pat of
  InL (PatVarF Nothing)     -> Just mempty
  InL (PatVarF (Just name)) -> Just $
    MatchResult Map.empty (Map.singleton name $ Fix tm)
  InR pat' -> case tm of
    InL _   -> Nothing
    InR tm' -> fMatches pat' tm'

-- | Whether the "target" functor matches.
fMatches
  :: (DomainFunctor f a, Show a)
  => f Text (Fix (PatVarF     :+: f Text))
  -> f a    (Fix (VarBindingF :+: f a   ))
  -> Maybe (MatchResult f a)
fMatches f1 f2 = do
  zipped <- bizipMatchWith (fmap Just . (,)) matches f1 f2
  pure $ bifoldMap
    (\(i, a) -> MatchResult (Map.singleton i a) mempty)
    id
    zipped

translate
  :: forall f g a m.
     ( DomainFunctor f a, CodomainFunctor g a
     , MonadWriter (Seq Text) m
     , Show a
     )
  => DenotationChart' (f Text) (MachineF :+: g Text)
  -> Fix (VarBindingF :+: f a)
  -> MaybeT m (Fix (VarBindingF :+: MachineF :+: g (Either Text a)))
translate chart tm = case unfix tm of
  InL (BindingF names subtm)
    -> Fix . InL . BindingF names <$> translate chart subtm
  InL (VarF name) -> pure $ Fix $ InL $ VarF name
  InR _ -> do
    tell1 $ "finding match for " <> tShow tm
    (MatchResult primVarBindings varBindings, rhs)
      <- MaybeT $ pure $ findMatch chart tm
    tell1 $ "varBindings: " <> tShow varBindings
    tell1 $ "primVarBindings: " <> tShow primVarBindings
    tell1 $ "rhs: " <> tShow rhs
    mapMaybeT (`runReaderT` TranslateEnv chart primVarBindings varBindings)
      (translate' rhs)

tShow :: Show a => a -> Text
tShow = Text.pack . show

tell1 :: MonadWriter (Seq Text) m => Text -> m ()
tell1 = tell . Seq.singleton

translate'
  :: ( DomainFunctor f a, CodomainFunctor g a
     , MonadWriter (Seq Text) m, MonadReader (TranslateEnv f g a) m
     , Show a
     )
  => Fix (VarBindingF :+: MeaningOfF :+: MachineF :+: g Text)
  -> MaybeT m (Fix (VarBindingF :+: MachineF :+: g (Either Text a)))
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
    translate'
    tm'

expectRight :: Show a => Either a b -> EvalM f b
expectRight = \case
  Left name -> throwError $
    "found (prim) pattern variable in purification " ++ show name
  Right a -> pure a

-- | substitute @arg@ for @name@ in @body@
subst
  :: Traversable f
  => Text
  -> Fix (VarBindingF :+: MachineF :+: f)
  -> Fix (VarBindingF :+: MachineF :+: f)
  -> EvalM f' (Fix (VarBindingF :+: MachineF :+: f))
subst name arg (Fix body) = case body of
  InL (VarF name')
    -> pure $ if name == name' then arg else Fix $ InL $ VarF name'
  InL (BindingF names body')
    -> Fix . InL . BindingF names <$> subst name arg body'
  InR f
    -> Fix . InR <$> traverse (subst name arg) f

eval'
  :: CodomainFunctor f a
  => Fix (VarBindingF :+: MachineF :+: f (Either Text a))
  -> EvalM (f a) (Fix (f a))
eval' (Fix f) = case f of
  InL BindingF{}  -> throwError $ "bare binding: " ++ show' f
  InL (VarF name) -> view (evalVarVals . at name)
    ??? "couldn't look up variable " ++ show name

  InR (InL (App (Fix (InR (InL (Lam (Fix (InL (BindingF [name] body))))))) arg)) -> do
    ret <- subst name arg body
    eval' ret
  InR (InL App{}) -> throwError $ "invalid app: " ++ show' f
  InR (InL Lam{}) -> throwError $ "bare lambda: " ++ show' f

  InR (InL (PrimApp name args)) -> do
    primApps <- view evalPrimApp
    fun      <- primApps name ?? "couldn't look up prim function " ++ show name
    args'    <- traverse eval' args
    pure $ Fix $ fun $ Seq.fromList $ unfix <$> args'

  InR (InR tm) -> Fix <$> bitraverse expectRight eval' tm

  where show' a = liftShowsPrec showsPrec showList 0 a ""
