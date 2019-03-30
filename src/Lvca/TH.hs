{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Lvca.TH (mkTypes, mkSyntaxInstances, Options(..), defOptions) where

import           Control.Lens                    (Prism', ifor, (<&>), prism', (^..), preview, review, _Just, findOf)
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad                   (join)
import           Data.Bifunctor.TH               hiding (Options)
import           Data.Data
import           Data.Eq.Deriving
import qualified Data.List                       as List
import           Data.Maybe                      (isJust)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Matchable.TH
import           Data.Monoid.Same                (Same(..), allSame)
import qualified Data.Set                        as Set
import           Data.Text                       (Text, pack, unpack)
import qualified Data.Text                       as Text
import           Data.Traversable                (for)
import           Language.Haskell.TH
import           Text.Megaparsec                 (runParser)
import           Text.Show.Deriving
import           Language.Haskell.TH.Syntax      (lift, dataToExpQ)

import           Lvca.FunctorUtil            (_Fix)
import           Lvca.ParseSyntaxDescription
import           Lvca.SyntaxComponents
import           Lvca.Types                  hiding (valences)
import qualified Lvca.Types                  as Types

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
    helpers <- mkTermHelpers (SyntaxChart sorts) (mkName' dtName) vars'
    let chartDefs = case mChartName of
          Nothing -> []
          Just chartName ->
            let chartNameName = mkName' chartName
            in [ SigD chartNameName ty
               , ValD (VarP chartNameName) (NormalB expr) []
               ]
    pure $ [ dataDecl ] <> chartDefs <> helpers

  pure $ concat decls

data VarTy = SortVar !Name | ExternalVar !Name

varTyVar :: VarTy -> PatQ
varTyVar = \case
  SortVar     v -> varP v
  ExternalVar v -> varP v

-- | Does this syntax chart ever refer to another sort?
hasSortVar :: SyntaxChart -> Bool
hasSortVar = isJust . findOf
  (syntaxChartContents . traverse . sortOperators . traverse . operatorArity .
   Types.valences . traverse . valenceSorts)
  (\case { SortAp{} -> True; _ -> False })

-- | Does this syntax chart ever refer to an external?
hasExternalVar :: SyntaxChart -> Bool
hasExternalVar = isJust . findOf
  (syntaxChartContents . traverse . sortOperators . traverse . operatorArity .
   Types.valences . traverse . valenceSorts)
  (\case { External{} -> True; _ -> False })

mkTermHelpers :: SyntaxChart -> Name -> [Text] -> Q [Dec]
mkTermHelpers chart@(SyntaxChart chartContents) fName vars = do
  let allVarNames = [1 :: Int ..] <&> \i -> mkName ("v" ++ show i)
      varNameGen = zipWith
        (\varName (Valence _ resultSort) -> case resultSort of
          SortAp{}   -> SortVar     varName
          External{} -> ExternalVar varName
        )
        allVarNames

      -- We only name patP' if it's used
      patP'Name = if hasSortVar chart then "patP'" else "_"

  -- we always declare rtl and ltr
  let mkPatPDecls =
        [ funD (mkName "rtl") [ clause [] (normalB $ lamCaseE $
            concat $
              chartContents ^.. traverse <&> \(SortDef _vars operators) ->
                operators <&> \(Operator name (Arity valences) _desc) ->
                  match
                    (conP
                      (mkName' name)
                      (varTyVar <$> varNameGen valences))
                    (normalB [|
                      PatternTm
                      $(litE $ StringL $ unpack name)
                      $(listE $ varNameGen valences <&> \case
                        SortVar     v -> [| review patP' $(varE v) |]
                        ExternalVar v -> [| review p     $(varE v) |])
                      |])
                    []
          ) [] ]
        , funD (mkName "ltr") [ clause [] (normalB $ lamCaseE $
            (concat $
              chartContents ^.. traverse <&> \(SortDef _vars operators) ->
                operators <&> \(Operator name (Arity valences) _desc) ->
                  match
                    [p| PatternTm
                      $(litP $ StringL $ unpack name)
                      $(listP $ varTyVar <$> varNameGen valences)
                    |]
                    (normalB $ foldl
                      (\con -> \case
                        SortVar     v -> [| $con <*> preview patP' $(varE v) |]
                        ExternalVar v -> [| $con <*> preview p     $(varE v) |])
                      [| pure $(conE $ mkName' name) |]
                      (varNameGen valences))
                    [])
            ++ [ match [p| _ |] (normalB [| Nothing |]) [] ]
          ) [] ]
        ]

  -- we only declare p when it's used, ie there are externals
  let mkPatPDecls' =
        -- , sigD (mkName "p") [t| forall a b. Prism (Pattern a) (Pattern b) a b |]
        [ sigD (mkName "p") [t| forall a. Prism' (Pattern a) a |]
        , valD (varP (mkName "p")) (normalB [| _PatternPrimVal . _Just |]) []
        ]

  -- for each sort:
  --   for each operator:
  --     emit a line like:
  --       ltr: `Plus a b -> PatternTm "Plus" [ review patP' a, review patP' b ]`
  --       rtl: `PatternTm "Plus" [ a, b ]
  --         -> Plus <$> preview patP' a <*> preview patP' b`
  patPDec <- funD (mkName' "mkPatP")
    [ clause [varP (mkName patP'Name)] (normalB [| prism' rtl ltr |]) $
      mkPatPDecls ++ if hasExternalVar chart then mkPatPDecls' else []
    ]

      -- We only name patP' if it's used
  let termP'Name = if hasSortVar chart then "termP'" else "_"

  let mkTermPDecls =
        [ funD (mkName "rtl") [ clause [] (normalB $ lamCaseE $
            concat $
              chartContents ^.. traverse <&> \(SortDef _vars operators) ->
                operators <&> \(Operator name (Arity valences) _desc) ->
                  match
                    (conP
                      (mkName' name)
                      (varTyVar <$> varNameGen valences))
                    (normalB [|
                      Term
                      $(litE $ StringL $ unpack name)
                      $(listE $ varNameGen valences <&> \case
                        SortVar     v -> [| review termP' $(varE v) |]
                        ExternalVar v -> [| review p      $(varE v) |])
                      |])
                    []
          ) [] ]
        , funD (mkName "ltr") [ clause [] (normalB $ lamCaseE $
            (concat $
              chartContents ^.. traverse <&> \(SortDef _vars operators) ->
                operators <&> \(Operator name (Arity valences) _desc) ->
                  match
                    [p| Term
                      $(litP $ StringL $ unpack name)
                      $(listP $ varTyVar <$> varNameGen valences)
                    |]
                    (normalB $ foldl
                      (\con -> \case
                        SortVar     v -> [| $con <*> preview termP' $(varE v) |]
                        ExternalVar v -> [| $con <*> preview p      $(varE v) |])
                      [| pure $(conE $ mkName' name) |]
                      (varNameGen valences))
                    [])
            ++ [ match [p| _ |] (normalB [| Nothing |]) [] ]
          ) [] ]
        ]

  -- we only declare p when it's used, ie there are externals
  let mkTermPDecls' =
        [ sigD (mkName "p") [t| forall a. Prism' (Term a) a |]
        , valD (varP (mkName "p")) (normalB [| _Fix . _PrimValueF |]) []
        ]

  -- for each sort:
  --   for each operator:
  --     emit a line like:
  --       ltr: `Plus a b -> Term "Plus" [ review termP' a, review termP' b ]`
  --       rtl: `Term "Plus" [ a, b ]
  --         -> pure Plus <*> preview termP' a <*> preview termP' b`
  termPDec <- funD (mkName' "mkTermP")
    [ clause [varP (mkName termP'Name)] (normalB [| prism' rtl ltr |]) $
      mkTermPDecls ++ if hasExternalVar chart then mkTermPDecls' else []
    ]

  let appliedCon = foldl
        (\con var -> appT con (varT (mkName' var)))
        (conT fName)
        vars

  pure []

mkSyntaxInstances :: Name -> Q [Dec]
mkSyntaxInstances dtName = fmap join $ sequence
  [ deriveBifunctor     dtName
  , deriveBifoldable    dtName
  , deriveBitraversable dtName
  , deriveShow1         dtName
  , deriveShow2         dtName
  , deriveEq1           dtName
  , deriveEq2           dtName
  , deriveBimatchable   dtName
  , makeLenses          dtName
  ]
