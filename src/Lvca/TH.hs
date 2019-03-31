{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Lvca.TH (mkTypes, mkSyntaxInstances, Options(..), defOptions) where

import           Control.Lens                    (ifor, (<&>))
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad                   (join)
import           Data.Data
import           Data.Eq.Deriving
import qualified Data.List                       as List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Monoid.Same                (Same(..), allSame)
import qualified Data.Set                        as Set
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text                       as Text
import           Data.Traversable                (for)
import           Language.Haskell.TH
import           Text.Megaparsec                 (runParser)
import           Text.Show.Deriving
import           Language.Haskell.TH.Syntax      (lift, dataToExpQ)

import           Lvca.ParseSyntaxDescription
import           Lvca.SyntaxComponents
import           Lvca.Types                  hiding (valences)

mkName' :: Text -> Name
mkName' = mkName . unpack

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'pack) <$> lift (unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)

defaultBang :: Bang
defaultBang = Bang NoSourceUnpackedness SourceStrict

data Options = Options
  { syntaxChartName :: !(Maybe Text)
  , externals       :: !(Map SortName (Q Type))
  }

defOptions :: Options
defOptions = Options Nothing Map.empty

mkTypes :: Options -> Text -> Q [Dec]
mkTypes (Options mChartName externals) tDesc = do
  let desc = runParser parseSyntaxDescription "(template haskell)" tDesc
  chart <- case desc of
    Left err   -> fail $ show err
    Right good -> pure good

  let primName = mkName' "prim"
      expName  = mkName' "exp"
      SyntaxComponents components _ = findChartComponents chart

  decls <- for components $ \(SyntaxComponent (SyntaxChart sorts) vars) -> do
    let vars' = case allSame vars of
          Same sameVars  -> sameVars
          DegenerateSame -> error "unexpected DegenerateSame in var check"
          NotSame a b    -> error $ "we can't currently handle a connected " ++
            "component of data types with different parameters: eg (" ++
            show a ++ ") / (" ++ show b ++ ")."

    let dtName = Text.concat $ Map.keys sorts
    ctors <- ifor sorts $ \_sortName (SortDef _vars ops) -> do
      let -- functorSortName = mkName' sortName
          sortSet = Set.fromList $ Map.keys sorts

          handleSort = \case
            SortAp name applicands
              | name `elem` sortSet
              -> VarT expName
              | name `List.elem` vars'
              -> VarT $ mkName' name
              | otherwise
              -> foldl AppT (ConT (mkName' name)) $
                fmap handleSort applicands ++
                [VarT primName, VarT expName]
            External name -> case Map.lookup name externals of
              Nothing  -> error $ "unhandled binding external: " ++ unpack name
              Just _ty -> VarT primName
          handleValence (Valence _ sort) = (defaultBang, handleSort sort)
          handleArity (Arity valences) = fmap handleValence valences

          sortCtors = ops <&> \case
            Operator name arity _
              -- errors if there are any duplicate names
              -> NormalC (mkName' name) (handleArity arity)

      pure sortCtors

    let dataDecl = DataD
          []
          (mkName' dtName)
          (fmap PlainTV $ fmap mkName' vars' ++ [primName, expName])
          Nothing
          (concat $ Map.elems ctors)
          [ DerivClause Nothing
            [ ConT ''Eq
            , ConT ''Show
            , ConT ''Functor
            , ConT ''Foldable
            , ConT ''Traversable
            ]
          ]

    ty      <- [t| SyntaxChart |]
    expr    <- [| SyntaxChart $(liftDataWithText sorts) |]
    let chartDefs = case mChartName of
          Nothing -> []
          Just chartName ->
            let chartNameName = mkName' chartName
            in [ SigD chartNameName ty
               , ValD (VarP chartNameName) (NormalB expr) []
               ]
    pure $ [ dataDecl ] <> chartDefs

  pure $ concat decls

mkSyntaxInstances :: Name -> Q [Dec]
mkSyntaxInstances dtName = fmap join $ sequence
  [ deriveShow1         dtName
  , deriveEq1           dtName
  , makeLenses          dtName
  ]
