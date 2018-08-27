{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.Proceed
  ( eval
  , proceed
  ) where

import           Control.Lens         hiding (from, to, (??))
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader
import           Data.Foldable        (toList)
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import           Data.Map.Strict      (Map)
import           Data.Maybe           (fromMaybe)
import qualified Data.Sequence        as Seq
import qualified Data.Text            as Text
import           Data.Text            (Text)
import           Data.Traversable     (for)

import           Linguist.Types
import           Linguist.Util        (pair, (??))


type EvalEnv a = (SyntaxChart, DenotationChart a, Map Text (Term a))

eval
  :: Show a
  => SortName
  -> SyntaxChart
  -> DenotationChart a
  -> Term a
  -> Either String (Term a)
eval sort sChart dChart tm
  = runReaderT (eval' sort tm) (sChart, dChart, Map.empty)

-- TODO: we're confused about sorts. here we basically ignore them.
-- PatternPrimVal includes a sort. All sorts are mixed up in dynamics.
-- findMatch should maybe return the sort it finds.
eval' :: Show a => SortName -> Term a -> ReaderT (EvalEnv a) (Either String) (Term a)
eval' sort = \case
  tm@(Term _name subtms) -> do
    (sChart, dChart, vars) <- ask
    let matchesEnv = MatchesEnv sChart sort vars
    case runReaderT (findMatch dChart tm) matchesEnv of
      Just (_assignment, CallForeign f) -> do
        subtms' <- traverse (eval' sort) subtms
        eval' sort $ f $ Seq.fromList subtms'
      Just (subst@(Subst assignment correspondences), BindIn bindings toSlot') -> do
        varVals <- for bindings $ \(patName, fromSlot) -> do
          from <- assignment ^? ix fromSlot ??
            "couldn't find assignment for variable " ++
              Text.unpack fromSlot ++ " in substitution " ++ show subst ++
              " derived from term " ++ show tm
          (_, varName) <- List.find ((== patName) . fst) correspondences ??
            "unable to find pattern name " ++ Text.unpack patName ++ " in " ++ show correspondences
          pure (varName, from)

        to <- assignment ^? ix toSlot' ??
          "couldn't find assignment for variable " ++
            Text.unpack toSlot' ++ " in substitution " ++ show subst ++
            " derived from term " ++ show tm

        local (_3 %~ Map.union (Map.fromList varVals)) (eval' sort to)
      Just (_, Value) -> pure tm

      Just (Subst assignment _, Cbv fun argSlots') -> do
        fun' <- assignment ^? ix fun ?? "TODO 1"
        -- TODO: generalize to multiple args
        (varNames, body) <- case fun' of
          Binding varNames body -> pure (varNames, body)
          tm'                   -> throwError $ show tm'
        args   <- for argSlots' $ \argSlot -> assignment ^? ix argSlot ?? "TODO 3"
        argTms <- traverse (eval' sort) args
        varVals <- pair varNames argTms ?? "TODO X"
        eval' sort $ substIn (Map.fromList varVals) body

      Just (Subst assignment _, Cbn fun argSlots') -> do
        fun' <- assignment ^? ix fun ?? "TODO 4"
        -- TODO: generalize to multiple args
        (varNames, body) <- case fun' of
          Binding varNames body -> pure (varNames, body)
          tm'                   -> throwError $ show tm'
        args  <- for argSlots' $ \argSlot -> assignment ^? ix argSlot ?? "TODO 6"
        varTms <- pair varNames args ?? "TODO X2"
        eval' sort $ substIn (Map.fromList varTms) body

      Nothing -> throwError $ "cound't find match for term " ++ show tm

  Binding{} -> throwError "can't evaluate exposed binding"
  Var name -> do
    tm <- view (_3 . at name)
    case tm of
      Just tm' -> pure tm'
      Nothing -> throwError $ "unable to look up var " ++ Text.unpack name
  tm@PrimValue{} -> pure tm

-- Only to be used in evaluation -- not proceed.
substIn :: Map Text (Term a) -> Term a -> Term a
substIn vals tm = case tm of
  Term name subtms -> Term name $ substIn vals <$> subtms
  Binding boundNames body -> case filter (`Map.notMember` vals) boundNames of
    [] -> body
    boundNames' -> Binding boundNames' $ substIn vals body
  Var name
    | Just val <- vals ^? ix name
    -> val
    | otherwise          -> tm
  PrimValue{} -> tm

type ProceedEnv a = (SyntaxChart, DenotationChart a)

proceed :: Show a => StateStep a -> Reader (ProceedEnv a) (StateStep a)
proceed (StateStep stack tmVal) = case tmVal of
  Descending tm@(Term name subtms) -> do
    (sChart, dChart) <- ask
    let env = MatchesEnv sChart "Exp" $ frameVals stack
    pure $ case runReaderT (findMatch dChart tm) env of
      Just (_assignment, CallForeign f) -> case subtms of
        tm':tms -> StateStep
          (CbvForeignFrame name Empty tms f : stack)
          (Descending tm')
        _ -> Errored "1"

      Just (Subst assignment correspondences, BindIn bindings toSlot') ->
        fromMaybe (Errored "BindIn") $ do
          varVals <- for bindings $ \(patName, fromSlot) -> do
            from <- assignment ^? ix fromSlot
            (_, varName) <- List.find ((== patName) . fst) correspondences
            pure (varName, from)
          to   <- assignment ^? ix toSlot'
          let frame = BindingFrame $ Map.fromList varVals
          Just $ StateStep (frame : stack) (Descending to)
      Just (Subst assignment _, Cbv body argNames) ->
        fromMaybe (Errored "Cbv") $ do
          body' <- assignment ^? ix body
          tms   <- for argNames $ \arg -> assignment ^? ix arg
          case tms of
            tm':tms' -> Just $ StateStep
              (CbvFrame name argNames Empty tms' body' : stack)
              (Descending tm')
            [] -> Just $ StateStep stack (Descending body')
      Just (Subst assignment _, Cbn body argNames) ->
        fromMaybe (Errored "Cbn") $ do
          body' <- assignment ^? ix body
          args  <- for argNames $ \arg -> assignment ^? ix arg
          let argVals = Map.fromList $ zip argNames args
          Just $ StateStep (BindingFrame argVals : stack)
            (Descending body')

      Just _ -> Errored "3"
      Nothing -> Errored $ Text.pack $ show tmVal

  Descending (Var name) -> pure $ case findBinding stack name of
    Just tmVal' -> StateStep stack (Descending tmVal')
    Nothing     -> Errored $ "couldn't find var: " <> name

  Descending (Binding _ _) -> error "TODO"

  -- reverse direction and return this value
  Descending val -> proceed (StateStep stack (Ascending val))

  Ascending val -> pure $ case stack of
    [] -> Done val
    CbvForeignFrame _name vals []       denote : stack' -> StateStep
      stack'
      (Ascending (denote (vals |> val)))
    CbvForeignFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvForeignFrame name (vals |> val) tms denote : stack')
      (Descending tm')
    CbvFrame _name argNames vals [] body : stack' ->
      let argVals = Map.fromList $ zip argNames $ toList vals
      in StateStep (BindingFrame argVals : stack') (Descending body)
    CbvFrame name argNames vals (tm':tms) body : stack' -> StateStep
      (CbvFrame name argNames (vals |> val) tms body : stack')
      (Descending tm')
    BindingFrame _ : stack' -> StateStep stack' tmVal

proceed e@Errored{} = pure e
proceed d@Done{}    = pure d
