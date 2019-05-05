{-# LANGUAGE TemplateHaskell #-}
module Lvca.ParseTerm
  ( Parser
  , ParseEnv(..)
  , parseChart
  , parseSort
  , standardParser
  , standardParser'
  ) where

import           Control.Lens         hiding (prism)
import           Control.Lens.Extras  (is)
import           Control.Monad.Reader
import           Data.Foldable        (asum, for_, toList)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Sequence        as Seq
import           Data.Text            (Text, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Lvca.ParseUtil
import           Lvca.Types     hiding (valence)
import           Lvca.Util

-- TODO: we're not actually using Err.
-- TODO: nice error messages
-- newtype Err = Err String
--   deriving (Eq, Ord, IsString)

-- instance ShowErrorComponent Err where
--   showErrorComponent (Err str) = str

data ParseEnv a = ParseEnv
  { _parseChart      :: !SyntaxChart
  , _parseSort       :: !Sort
  }
makeLenses ''ParseEnv

type Parser a b = ReaderT (ParseEnv a) (Parsec Void Text) b

-- | Parse a term using the standard syntax, consuming the entire file.
standardParser' :: forall a. Parser a Term
standardParser' = standardParser <* eof

-- | Generates a parser for any language parsing a standard syntax. Example:
--
-- > Add(
-- >   Times(1; 2);
-- >   Ap(
-- >     Lam(x. Times(x; x));
-- >     3
-- >   )
-- > )
standardParser :: forall a. Parser a Term
standardParser = do
  SyntaxChart syntax <- view parseChart

  let sortParsers :: Map SortName (Map Text Sort -> Parser a Term)
      sortParsers = syntax <@&> \sortName (SortDef _vars ops) concreteSorts ->

        -- build a parser for each operator in this sort
        let opParsers = ops <&> \case
              Operator name (FixedArity valences') _concreteSyntax
                -> label (unpack name) $ do
                  _ <- try $ do
                    _ <- string name
                    notFollowedBy $ alphaNumChar <|> char '\'' <|> char '_'

                  let valences'' = valenceSubst concreteSorts <$> valences'

                  subTms <- parens $ case valences'' of
                    []   -> pure Seq.empty
                    v:vs -> foldl
                      (\parseLeft valence -> do
                        pvs <- parseLeft
                        _   <- symbol ";"
                        pv  <- parseValence parseTerm valence
                        pure $ pvs |> pv)
                      (Seq.singleton <$> parseValence parseTerm v)
                      vs
                  -- TODO: convert Term to just use Sequence
                  pure $ Term name $ toList subTms

              Operator _name (VariableArity _index _valence) _concreteSyntax
                -> error "TODO"

        in asum opParsers <?> unpack sortName ++ " operator"

      -- parse an operator in the current sort or a variable
      parseTerm = do
        SortAp sortHead sortArgs <- view parseSort
        let SortDef sortVars _ = syntax ^?! ix sortHead
            sortVarVals = Map.fromList (zip sortVars sortArgs)

        case sortParsers ^? ix sortHead of
          Just sortOpParsers
            -> sortOpParsers sortVarVals <|> fmap Var parseName
          Nothing -> fail $
            "unable to find sort " <> unpack sortHead <> " among " <>
            show (Map.keys sortParsers)

  parseTerm

parseValence :: Parser a Term -> Valence -> Parser a Scope
parseValence parseTerm valence@(FixedValence sorts (NamedSort _ bodySort)) = do
  let parseTerm' = local (parseSort .~ bodySort) parseTerm

  label ("valence " <> show valence) $ Scope
    <$> countSepBy (length sorts) parseName (symbol ".")
    <*> parseTerm'
