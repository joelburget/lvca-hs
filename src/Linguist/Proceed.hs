{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.Proceed where

import           Control.Lens         hiding (from, to)
import           Control.Monad.Reader
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as Text

import           Linguist.Types


proceed :: Show a => DenotationChart a -> StateStep a -> StateStep a
proceed chart (StateStep stack tmVal) = case tmVal of
  Left (Term name subtms) ->
    let env = MatchesEnv eChart "Exp" (frameVals stack)
    in case runReaderT (findMatch chart tmVal) env of
      Just (_assignment, CallForeign f) -> case subtms of
        tm':tms -> StateStep
          (CbvForeignFrame name Empty tms f : stack)
          (Left tm')
        _ -> Errored "1"

      Just (assignment, BindIn name' from to) ->
        fromMaybe (Errored "BindIn") $ do
          -- XXX we should get this from the substitution
          -- Var name' <- subtms ^? ix (_ name')
          from' <- assignment ^? ix from
          to'   <- assignment ^? ix to
          -- traceM $ "from: " ++ show from'
          -- traceM $ "to: " ++ show to'
          let frame = BindingFrame $ Map.singleton name' from'
          Just $ StateStep (frame : stack) to'
      Just (assignment, Cbv body arg) ->
        fromMaybe (Errored "Cbv") $ do
          body' <- assignment ^? ix body
          arg'  <- assignment ^? ix arg
          case body' of
            Right _     -> Just $ Errored "found value not term in Cbv body"
            Left body'' -> Just $ StateStep (CbvFrame arg body'' : stack) arg'
      Just (assignment, Cbn body arg) ->
        fromMaybe (Errored "Cbn") $ do
          body' <- assignment ^? ix body
          arg'  <- assignment ^? ix arg
          case body' of
            Right _     -> Just $ Errored "found value not term in Cbn body"
            Left body'' -> Just $
              StateStep (BindingFrame (Map.singleton arg arg') : stack) (Left body'')

      Just _ -> Errored "3"
      Nothing -> Errored $ Text.pack $ show tmVal

  Left (Var name) -> case findBinding stack name of
    Just tmVal' -> StateStep stack tmVal'
    Nothing     -> Errored $ "couldn't find var: " <> name

  Left (PrimTerm primTm) -> case stack of
    CbvForeignFrame _ vals [] f : stack' ->
      case f (vals |> PrimValue primTm) of
        result -> StateStep stack' (Right result)
    CbvForeignFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvForeignFrame name (vals |> PrimValue primTm) tms denote : stack')
      (Left tm')
    CbvFrame _ _ : _ -> Errored "cbv frame over prim term"
    BindingFrame  _ : stack' -> StateStep stack' tmVal
    [] -> Errored "empty stack with term"

  -- reverse direction and return this value
  Left (Return val) -> proceed chart (StateStep stack (Right val))
  -- Right (Thunk tm)  -> proceed chart (StateStep stack (Left tm))

  Left (Binding _ _) -> error "TODO"

  Right val -> case stack of
    [] -> Done val
    CbvForeignFrame _name vals []       denote : stack' -> StateStep
      stack'
      (Right (denote (vals |> val)))
    CbvForeignFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvForeignFrame name (vals |> val) tms denote : stack')
      (Left tm')
    CbvFrame argName body : stack' -> StateStep
      (BindingFrame (Map.singleton argName (Right val)) : stack')
      (Left body)
    BindingFrame _ : stack' -> StateStep stack' tmVal

proceed _ e@Errored{} = e
proceed _ d@Done{} = d
