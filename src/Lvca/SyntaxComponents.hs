{-# LANGUAGE TemplateHaskell #-}

module Lvca.SyntaxComponents
  (
  -- ** Syntax components
    SyntaxComponent(..)
  , componentChart
  , componentVars
  , SyntaxComponents(..)
  , syntaxComponents
  , componentMapping
  , findChartComponents
  ) where

import           Control.Lens    hiding (mapping, op, prism)
import           Data.Graph      (SCC(..), stronglyConnCompR)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)

import           Lvca.Types      hiding (valences)
import           Lvca.Util

data SyntaxComponent = SyntaxComponent
  { _componentChart :: !SyntaxChart
  -- ^ A sub-syntax chart, representing a strongly-connected subset of the
  -- original chart
  , _componentVars  :: ![[Text]]
  -- ^ All the variable names that occur as a parameter to any type in this
  -- component
  }

-- | It's sometimes useful to break a syntax chart into its connected
-- components, which contain data types dependent on each other, and which
-- become individual compilation units.
data SyntaxComponents = SyntaxComponents
  { _syntaxComponents :: ![SyntaxComponent]
  -- ^ List of sub-syntax charts, each representing a strongly-connected subset
  -- of the original chart
  , _componentMapping :: !(Map SortName SyntaxComponent)
  -- ^ Map from the originally defined sorts to the component they occur in
  }

-- | The list of sorts this sort (directly) depends on
sortDeps :: Sort -> [SortName]
sortDeps (SortAp name sorts) = name : concatMap sortDeps sorts

-- | Find the strongly connected components / compilation units of a family of
-- data types.
findChartComponents :: SyntaxChart -> SyntaxComponents
findChartComponents (SyntaxChart sorts) =
  let sortsWithDeps :: [(SortDef, SortName, [SortName])]
      sortsWithDeps = Map.toList sorts <&> \(name, sortDef@(SortDef _ ops)) ->
        (sortDef,name,) $ concat3 $
          -- XXX partial
          ops <&> \(Operator _name (FixedArity valences) _syntax) ->
            -- XXX partial
            valences <&> \(FixedValence vSorts result) ->
              fmap (sortDeps . _namedSort) $ result : vSorts

      components :: [SCC (SortDef, SortName, [SortName])]
      components = stronglyConnCompR sortsWithDeps

      (components', mappings) = unzip $ components <&> \scc ->
        let preChart :: [(SortName, SortDef)]
            (preChart, names) = case scc of
              AcyclicSCC (sortDef, name, _) ->
                ( [(name, sortDef)]
                , [name]
                )
              CyclicSCC vertices ->
                ( vertices <&> (\(sortDef, name, _) -> (name, sortDef))
                , vertices ^.. traverse . _2
                )

            -- Collect all the variable names that occur in any sorts
            vars = preChart <&> \(_, SortDef sortVars _) -> sortVars

            component = SyntaxComponent
              (SyntaxChart (Map.fromList preChart))
              vars
        in (component, Map.fromList $ zip names $ repeat component)
  in SyntaxComponents components' (Map.unions mappings)

makeLenses ''SyntaxComponent
makeLenses ''SyntaxComponents
