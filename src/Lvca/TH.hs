{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Lvca.TH (mkTypes, mkSyntaxInstances, Options(..), defOptions) where

import           Control.Lens                    (Prism', ifor, (<&>), prism', (^..), preview, review, _Just)
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad                   (join)
import           Data.Bifunctor.TH               hiding (Options)
import           Data.Data
import           Data.Eq.Deriving
import qualified Data.List                       as List
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Matchable.TH
import           Data.Text                       (Text, pack, unpack)
import           Language.Haskell.TH
import           Text.Megaparsec                 (runParser)
import           Text.Show.Deriving
import           Language.Haskell.TH.Syntax      (lift, dataToExpQ)

import           Lvca.FunctorUtil            (Fix(Fix), _Fix)
import           Lvca.ParseSyntaxDescription
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
  { dataTypeName    :: !Text
  , syntaxChartName :: !(Maybe Text)
  , externals       :: !(Map SortName (Q Type))
  }

defOptions :: Options
defOptions = Options (error "must define a data type name") Nothing Map.empty

mkTypes :: Options -> Text -> Q [Dec]
mkTypes (Options dtName mChartName externals) tDesc = do
  let desc = runParser parseSyntaxDescription "(template haskell)" tDesc
  SyntaxChart sorts <- case desc of
    Left err   -> fail $ show err
    Right good -> pure good

  let primName        = mkName' "prim"
      expName         = mkName' "exp"

    -- let baseSortName = mkName' sortName
    --     baseVars     = PlainTV . mkName' <$> vars

    --     baseMkCon t = (if t `List.elem` vars then VarT else ConT) (mkName' t)
    --     baseHandleSort = \case
    --       SortAp name [] -> baseMkCon name
    --       SortAp name applicands
    --         -> foldl AppT (baseMkCon name) (fmap baseHandleSort applicands)
    --       External name -> case Map.lookup name externals of
    --         Nothing -> error $ "unhandled binding external: " ++ unpack name
    --         Just ty -> ty
    --     baseHandleValence (Valence _ sort) = (defaultBang, baseHandleSort sort)
    --     baseHandleArity (Arity valences) = fmap baseHandleValence valences

    --     baseOps = ops <&> \case
    --       Operator name arity _ -> NormalC (mkName' name) (baseHandleArity arity)

  ctors <- ifor sorts $ \sortName (SortDef vars ops) -> do
    let functorSortName = mkName' sortName
        -- functorVars     = fmap PlainTV $ (mkName' <$> vars) ++ [primName, expName]

        mkCon :: Text -> Type
        mkCon t =
          if | t `List.elem` vars
             -> VarT $ mkName' t
             | t == sortName
             -> VarT expName
             | otherwise
             -> ConT functorSortName
        handleSort = \case
          SortAp name [] -> mkCon name
          SortAp name applicands
            -> foldl AppT (mkCon name) (fmap handleSort applicands)
          External name -> case Map.lookup name externals of
            Nothing  -> error $ "unhandled binding external: " ++ unpack name
            Just _ty -> VarT primName
        handleValence (Valence _ sort) = (defaultBang, handleSort sort)
        handleArity (Arity valences) = fmap handleValence valences

        sortCtors = ops <&> \case
          Operator name arity _
            -- errors if there are any duplicate names
            -> NormalC (mkName' name) (handleArity arity)

    -- let decls =
    --       -- [ DataD [] baseSortName    baseVars    Nothing baseOps    []
    --       [ DataD [] functorSortName functorVars Nothing functorOps []
    --       ]
    pure sortCtors

  let dataDecl = DataD
        []
        (mkName' dtName)
        (fmap PlainTV [primName, expName])
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
  helpers <- mkTermHelpers (SyntaxChart sorts) (mkName' dtName)
  let chartDefs = case mChartName of
        Nothing -> []
        Just chartName ->
          let chartNameName = mkName' chartName
          in [ SigD chartNameName ty
             , ValD (VarP chartNameName) (NormalB expr) []
             ]
  pure $ [ dataDecl ] <> chartDefs <> helpers

data VarTy = SortVar Name | ExternalVar Name

varTyVar :: VarTy -> PatQ
varTyVar = \case
  SortVar     v -> varP v
  ExternalVar v -> varP v

mkTermHelpers :: SyntaxChart -> Name -> Q [Dec]
mkTermHelpers chart@(SyntaxChart chartContents) fName = do
  let allVarNames = [1 :: Int ..] <&> \i -> mkName ("v" ++ show i)
      varNameGen = zipWith
        (\varName (Valence _ resultSort) -> case resultSort of
          SortAp{}   -> SortVar     varName
          External{} -> ExternalVar varName
        )
        allVarNames

  syntaxDec <- funD (mkName "syntaxOf")
    [ clause [wildP] (normalB (liftDataWithText chart)) [] ]

--   patPSig <- sigD (mkName' "mkPatP")
--     [t| Prism' (Pattern $(varT a))                          (Fix $(varT f))
--      -> Prism' (Pattern $(varT a)) ($(conT fName) $(varT a) (Fix $(varT f)))
--     |]

  -- for each sort:
  --   for each operator:
  --     emit a line like:
  --       ltr: `Plus a b -> PatternTm "Plus" [ review patP' a, review patP' b ]`
  --       rtl: `PatternTm "Plus" [ a, b ]
  --         -> Plus <$> preview patP' a <*> preview patP' b`
  patPDec <- funD (mkName' "mkPatP")
    [ clause [varP (mkName "patP'")] (normalB [| prism' rtl ltr |])
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
          concat $
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
                  []
        ) [] ]

      -- , sigD (mkName "p") [t| forall a b. Prism (Pattern a) (Pattern b) a b |]
      , sigD (mkName "p") [t| forall a. Prism' (Pattern a) a |]
      , valD (varP (mkName "p")) (normalB [| _PatternPrimVal . _Just |]) []

      -- hide the warning about `p` not being used if there are no externals.
      -- , sigD (mkName "_unused") [t| forall a b. Prism (Pattern a) (Pattern b) a b |]
      , sigD (mkName "_unused") [t| forall a. Prism' (Pattern a) a |]
      , valD (varP (mkName "_unused")) (normalB [| p |]) []
      ]
    ]

--   termPSig <- sigD (mkName' "mkTermP")
--     [t| Prism' (Term $(varT a))                          (Fix $(varT f))
--      -> Prism' (Term $(varT a)) ($(conT fName) $(varT a) (Fix $(varT f)))
--     |]

  -- for each sort:
  --   for each operator:
  --     emit a line like:
  --       ltr: `Plus a b -> Fix (Term "Plus" [ review termP' a, review termP' b ])`
  --       rtl: `Fix (Term "Plus" [ a, b ])
  --         -> pure Plus <*> preview termP' a <*> preview termP' b`
  termPDec <- funD (mkName' "mkTermP")
    [ clause [varP (mkName "termP'")] (normalB [| prism' rtl ltr |])
      [ funD (mkName "rtl") [ clause [] (normalB $ lamCaseE $
          concat $
            chartContents ^.. traverse <&> \(SortDef _vars operators) ->
              operators <&> \(Operator name (Arity valences) _desc) ->
                match
                  (conP
                    (mkName' name)
                    (varTyVar <$> varNameGen valences))
                  (normalB [|
                    Fix (Term
                    $(litE $ StringL $ unpack name)
                    $(listE $ varNameGen valences <&> \case
                      SortVar     v -> [| review termP' $(varE v) |]
                      ExternalVar v -> [| review p      $(varE v) |])
                    ) |])
                  []
        ) [] ]
      , funD (mkName "ltr") [ clause [] (normalB $ lamCaseE $
          concat $
            chartContents ^.. traverse <&> \(SortDef _vars operators) ->
              operators <&> \(Operator name (Arity valences) _desc) ->
                match
                  [p| Fix (Term
                    $(litP $ StringL $ unpack name)
                    $(listP $ varTyVar <$> varNameGen valences)
                  ) |]
                  (normalB $ foldl
                    (\con -> \case
                      SortVar     v -> [| $con <*> preview termP' $(varE v) |]
                      ExternalVar v -> [| $con <*> preview p      $(varE v) |])
                    [| pure $(conE $ mkName' name) |]
                    (varNameGen valences))
                  []
        ) [] ]

      -- , sigD (mkName "p") [t| forall a b. Prism (Term a) (Term b) a b |]
      , sigD (mkName "p") [t| forall a. Prism' (Term a) a |]
      , valD (varP (mkName "p")) (normalB [| _Fix . _PrimValue |]) []

      -- hide the warning about `p` not being used if there are no externals.
      -- , sigD (mkName "_unused") [t| forall a b. Prism (Term a) (Term b) a b |]
      , sigD (mkName "_unused") [t| forall a. Prism' (Term a) a |]
      , valD (varP (mkName "_unused")) (normalB [| p |]) []
      ]
    ]

  inst <- instanceD (pure []) [t| TermRepresentable $(conT fName) |]
    (fmap pure [ syntaxDec, patPDec, termPDec ])
  pure [ inst ]

mkSyntaxInstances :: Name -> Q [Dec]
mkSyntaxInstances dtName = fmap join $ sequence
  [ deriveBifunctor         dtName
  , deriveBifoldable        dtName
  , deriveBitraversable     dtName
  , deriveShow1             dtName
  , deriveShow2             dtName
  , deriveEq1               dtName
  , deriveEq2               dtName
  , deriveBimatchable       dtName
  , makeLenses              dtName
  -- , deriveTermRepresentable dtName chart
  ]
