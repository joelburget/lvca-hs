module Main where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc (hardline)
import Data.Text.Prettyprint.Doc.Render.Text
import System.Environment (getArgs)
import System.Exit
import           Text.Earley                           (fullParses)
import Text.Megaparsec
import System.Console.Haskeline

import Lvca.Bidirectional hiding (name)
import Lvca.Core
import Lvca.DenotationChart
import Lvca.EarleyParseTerm
import Lvca.ParseLanguage
import Lvca.Printer (prettyTm)
import Lvca.Types (keywords)
import Lvca.TokenizeConcrete (tokenizeConcrete)

interpretLanguage :: Lang -> IO ()
interpretLanguage lang@(Lang name _ _ _ _) = runInputT settings loop where
  historyFileName = ".futamura-" ++ Text.unpack name
  settings        = Settings noCompletion (Just historyFileName) True
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine $ Text.unpack name ++ "> "
    case minput of
      Nothing     -> return ()
      Just input  -> do
        liftIO $ do
          it <- runExceptT $ eval lang $ Text.pack input
          case it of
            Left err -> putStrLn err
            Right () -> pure ()
        loop

eval :: Lang -> Text -> ExceptT String IO ()
eval (Lang _name _abstractSyntax concreteSyntax statics dynamics) prog = do

  tmTokens <- case tokenizeConcrete (keywords concreteSyntax) prog of
    Left err   -> throwError $ errorBundlePretty err
    Right good -> pure good

  parsedTm <- case fullParses (concreteParser concreteSyntax) tmTokens of
    ([parsedTm], _)  -> pure parsedTm
    (parses, report) -> throwError $ unlines $
      [ "failed to parse term:"
      , show (length parses) ++ " parses found:"
      , show parses
      , show report
      ]

  case runCheck (Env statics Map.empty) (infer (convert [] parsedTm)) of
    Left err -> throwError $ "failed to typecheck:\n" ++ err
    Right ty -> liftIO $ putStrLn $ "inferred type: " ++ show ty -- TODO: pretty

  core <- case runReaderT (termToCore parsedTm) dynamics of
    Left err   -> throwError $ "failed to translate term to core:\n" ++ err
    Right core -> pure core

  val <- case evalCore core of
    Left msg  -> throwError msg
    Right val -> pure val

  valTm <- case valToTerm val of
    Nothing -> throwError "failed to translate value to term"
    Just tm -> pure tm

  liftIO $ putDoc $ runReader (prettyTm valTm) (0, concreteSyntax) <> hardline

main :: IO ()
main = do
  args <- getArgs
  languageFile <- case args of
    [languageFile] -> pure languageFile
    _              -> die "expected one argument (language file)"

  langFileContents <- TIO.readFile languageFile
  lang <- case runParser parseLang languageFile langFileContents of
    Left bad -> die (errorBundlePretty bad)
    Right good -> pure good

  interpretLanguage lang
