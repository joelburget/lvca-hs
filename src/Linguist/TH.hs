{-# language LambdaCase #-}
{-# language TemplateHaskell #-}

module Linguist.TH where

import Control.Lens (ifor, (<&>))
import Control.Lens.TH
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Language.Haskell.TH
import           Text.Megaparsec                 (runParser)

import Linguist.ParseSyntaxDescription
import Linguist.Types

mkName' :: Text -> Name
mkName' = mkName . unpack

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness SourceStrict

mkTypes :: Text -> Q [Dec]
mkTypes tDesc = do
  let desc = runParser parseSyntaxDescription "(template haskell)" tDesc
  SyntaxChart sorts <- case desc of
    Left err   -> fail $ show err
    Right good -> pure good

  decls <- ifor sorts $ \sortName (SortDef vars ops) -> do
    let sortName' = mkName' sortName
        vars' = fmap (PlainTV . mkName') vars

        mkCon = ConT . mkName'
        handleSort = \case
          SortAp name [] -> mkCon name
          SortAp name applicands
            -> foldr AppT (mkCon name) (fmap handleSort applicands)
        handleValence = \case
          Valence [] sort -> (defaultBang, handleSort sort)
          Valence _ _     -> error "unhandled binding valence"
          External _      -> error "unhandled binding valence"
        handleArity (Arity valences) = fmap handleValence valences
        ops' = ops <&> \(Operator name arity _)
          -> NormalC (mkName' name) (handleArity arity)
    lensDecls <- makeLenses sortName'
    pure $ DataD [] sortName' vars' Nothing ops' [] : lensDecls

  pure $ concat $ Map.elems decls
