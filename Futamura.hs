module Main where

import Control.Monad.Reader
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

import Lvca.Core
import Lvca.DenotationChart
import Lvca.EarleyParseTerm
import Lvca.ParseLanguage
import Lvca.Printer (prettyTm)
import Lvca.Types (_startSort, keywords)
import Lvca.TokenizeConcrete (tokenizeConcrete)

interpretLanguage :: Lang -> IO ()
interpretLanguage lang = runInputT defaultSettings $ loop where
  loop :: InputT IO ()
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing     -> return ()
      Just input  -> do
        liftIO $ eval lang $ Text.pack input
        loop

eval :: Lang -> Text -> IO ()
eval (Lang _name abstractSyntax concreteSyntax statics dynamics) prog = do

  -- putStrLn $ "concrete syntax:\n" ++ show concreteSyntax

  tmTokens <- case tokenizeConcrete (keywords concreteSyntax) prog of
    Left err   -> die $ errorBundlePretty err
    Right good -> pure good

  -- putStrLn $ "tokens: " ++ show tmTokens

  parsedTm <- case fullParses (concreteParser concreteSyntax) tmTokens of
    ([parsedTm], _)  -> pure parsedTm
    (parses, report) -> die $ unlines $
      [ "failed to parse term:"
      , show (length parses) ++ " parses found:"
      , show parses
      , show report
      ]

  -- putStrLn $ "parsed term: " ++ show parsedTm

  -- TODO: typecheck!
  -- runCheck (Env statics Map.empty) (check parsedTm)

  let mCore = runReaderT (termToCore parsedTm) dynamics

  core <- case mCore of
    Left err   -> die $ "failed to translate term to core:\n" ++ err
    Right core -> pure core

  val <- case evalCore core of
    Left msg  -> die msg
    Right val -> pure val

  valTm <- case valToTerm val of
    Nothing -> die "failed to translate value to term"
    Just tm -> pure tm

  putDoc $ runReader (prettyTm valTm) (0, concreteSyntax) <> hardline

main :: IO ()
main = do
  args <- getArgs
  languageFile <- case args of
    [languageFile] -> pure languageFile
    _              -> die "expected one argument (language file)"

  langFileContents <- TIO.readFile languageFile
  lang
    <- case runParser parseLang languageFile langFileContents of
      Left bad -> die (errorBundlePretty bad)
      Right good -> pure good

  interpretLanguage lang
