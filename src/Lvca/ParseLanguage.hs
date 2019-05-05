module Lvca.ParseLanguage where

import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import Data.Traversable (for)

import qualified Lvca.Bidirectional as Bidir
import Lvca.DenotationChart
import qualified Lvca.Types as Core
import Lvca.ParseBidirectional
import qualified Lvca.ParseConcreteSyntaxDescription as SD
import Lvca.ParseDenotationChart
-- import Lvca.ParseSyntaxDescription
import Lvca.ParseUtil (symbol, symbol', parseName, sc, scn)

type LanguageParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

data Lang = Lang
  { _languageName   :: !Text
  , _abstractSyntax :: !Core.SyntaxChart
  , _statics        :: ![Bidir.Rule]
  , _dynamics       :: !DenotationChart
  }

-- TODO: VariableValence
translateValence :: SD.AbstractValence -> Either String Core.Valence
translateValence (SD.AbstractValence args result) = Core.FixedValence
  <$> traverse translateNamedSort args
  <*> translateNamedSort result

translateNamedSort :: SD.NamedSort -> Either String Core.NamedSort
translateNamedSort (SD.NamedSort mName sort mIndex) = do
  sort' <- translateSort sort
  case mName of
    Just name -> pure $ Core.NamedSort name sort'
    Nothing   -> case sort' of
      Core.SortAp name [] -> pure $ Core.NamedSort name sort'
      _                   -> Left "TODO: NamedSort"

translateSort :: SD.Sort -> Either String Core.Sort
translateSort (SD.SortName name) = pure $ Core.SortAp name []
translateSort SD.SortAp{} = Left "TODO: SortAp"

translateTokens :: [SD.NonterminalToken] -> Either String Core.MixfixDirective
translateTokens = \case
  [SD.StringLiteral str] -> pure $ Core.Literal str
  -- XXX

translateLayout :: SD.NonterminalMatch -> Either String Core.OperatorDirective
translateLayout = \case
  SD.AssociativeMatch assoc -> pure $ Core.AssocDirective assoc
  SD.InfixMatch name fixity -> pure $ Core.InfixDirective name fixity
  SD.MixfixMatch toks -> Core.MixfixDirective <$> translateTokens toks

translateCtor :: SD.NonterminalCtor -> Either String Core.Operator
translateCtor (SD.NonterminalCtor (SD.AbstractPat name mNat valences) matches) = do
  layouts <- traverse translateLayout matches
  arity <- case mNat of
    Nothing -> Core.FixedArity <$> traverse translateValence valences
    Just (SD.Freenat n) -> case valences of
      [valence] -> Core.VariableArity n <$> translateValence valence
      _ -> Left "TODO"
  pure $ Core.Operator name arity layouts

translateSyntaxDesc :: SD.ConcreteSyntax -> Either String Core.SyntaxChart
translateSyntaxDesc (SD.ConcreteSyntax terminals nonterminals)
  = fmap (Core.SyntaxChart . Map.fromList) $
    for nonterminals $ \(SD.NonterminalRule sortName args ctors) -> do
      operators <- for ctors translateCtor
      pure (sortName, Core.SortDef args operators)

parseHeader :: MonadParsec e Text m => m b -> m b
parseHeader parseBody = symbol' "=" *> parseBody <* sc <* symbol "="

parseSubheader :: MonadParsec e Text m => m b -> m b
parseSubheader parseBody = symbol' "==" *> parseBody <* sc <* symbol "=="

parseLang :: LanguageParser Lang
parseLang = do
  name <- parseHeader $ string "language " >> parseName

  _ <- parseSubheader $ string "abstract syntax"
  syntaxDesc <- SD.syntaxDescription
  _ <- scn

  abstractSyntax <- case translateSyntaxDesc syntaxDesc of
    Left msg -> error msg
    Right s  -> pure s

  _       <- parseSubheader $ string "statics"
  statics <- parseBidirectional

  _        <- parseSubheader $ string "dynamics"
  dynamics <- parseDenotationChart

  pure $ Lang name abstractSyntax statics dynamics
