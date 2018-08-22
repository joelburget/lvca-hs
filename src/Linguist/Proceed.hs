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
  Descending tm@(Term name subtms) ->
    let env = MatchesEnv eChart "Exp" $ frameVals stack
    in case runReaderT (findMatch chart tm) env of
      Just (_assignment, CallForeign f) -> case subtms of
        tm':tms -> StateStep
          (CbvForeignFrame name Empty tms f : stack)
          (Descending tm')
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
          Just $ StateStep (frame : stack) (Descending to')
      Just (assignment, Cbv body arg) ->
        fromMaybe (Errored "Cbv") $ do
          body' <- assignment ^? ix body
          arg'  <- assignment ^? ix arg
          Just $ StateStep (CbvFrame name arg body' : stack) (Descending arg')
      Just (assignment, Cbn body arg) ->
        fromMaybe (Errored "Cbn") $ do
          body' <- assignment ^? ix body
          arg'  <- assignment ^? ix arg
          Just $ StateStep (BindingFrame (Map.singleton arg arg') : stack)
            (Descending body')

      Just _ -> Errored "3"
      Nothing -> Errored $ Text.pack $ show tmVal

  Descending (Var name) -> case findBinding stack name of
    Just tmVal' -> StateStep stack (Descending tmVal')
    Nothing     -> Errored $ "couldn't find var: " <> name

  Descending (Binding _ _) -> error "TODO"

  -- reverse direction and return this value
  Descending val -> proceed chart (StateStep stack (Ascending val))

  Ascending val -> case stack of
    [] -> Done val
    CbvForeignFrame _name vals []       denote : stack' -> StateStep
      stack'
      (Ascending (denote (vals |> val)))
    CbvForeignFrame name vals (tm':tms) denote : stack' -> StateStep
      (CbvForeignFrame name (vals |> val) tms denote : stack')
      (Descending tm')
    CbvFrame _name argName body : stack' -> StateStep
      (BindingFrame (Map.singleton argName val) : stack')
      (Descending body)
    BindingFrame _ : stack' -> StateStep stack' tmVal

proceed _ e@Errored{} = e
proceed _ d@Done{} = d
