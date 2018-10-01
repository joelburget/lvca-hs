{-# LANGUAGE TemplateHaskell #-}

module Linguist.TH where

import           Control.Lens                    (ifor, (<&>))
import qualified Data.List                       as List
-- import Control.Lens.TH (makeLenses)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Text                       (Text, unpack)
import           Language.Haskell.TH
import           Text.Megaparsec                 (runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types

mkName' :: Text -> Name
mkName' = mkName . unpack

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness SourceStrict

mkTypes :: Text -> Map SortName Type -> Q [Dec]
mkTypes tDesc externals = do
  let desc = runParser parseSyntaxDescription "(template haskell)" tDesc
  SyntaxChart sorts <- case desc of
    Left err   -> fail $ show err
    Right good -> pure good

  decls <- ifor sorts $ \sortName (SortDef vars ops) -> do
    let sortName' = mkName' sortName
        vars' = fmap (PlainTV . mkName') vars

        mkCon t =
          (if t `List.elem` vars then VarT else ConT) (mkName' t)
        handleSort = \case
          SortAp name [] -> mkCon name
          SortAp name applicands
            -> foldl AppT (mkCon name) (fmap handleSort applicands)
        handleValence = \case
          Valence _ sort -> (defaultBang, handleSort sort)
          External name  -> case Map.lookup name externals of
            Nothing -> error $ "unhandled binding external: " ++ unpack name
            Just ty -> (defaultBang, ty)
        handleArity (Arity valences) = fmap handleValence valences
        ops' = ops <&> \(Operator name arity _)
          -> NormalC (mkName' name) (handleArity arity)
    -- lensDecls <- makeLenses sortName'
    -- pure $ DataD [] sortName' vars' Nothing ops' [] : lensDecls
    pure [DataD [] sortName' vars' Nothing ops' []]

  pure $ concat $ Map.elems decls
