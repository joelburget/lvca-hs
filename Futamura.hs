module Main where

import Control.Monad.Reader
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc (hardline)
import Data.Text.Prettyprint.Doc.Render.Text
import System.Environment (getArgs)
import System.Exit
import           Text.Earley                           (fullParses)
import Text.Megaparsec

import Lvca.Core
import Lvca.DenotationChart
import Lvca.EarleyParseTerm
import Lvca.ParseLanguage
import Lvca.Printer (prettyTm)
import Lvca.Types (_startSort, keywords)
import Lvca.TokenizeConcrete (tokenizeConcrete)

main :: IO ()
main = do
  args <- getArgs
  (languageFile, prog) <- case args of
    [languageFile, prog] -> pure (languageFile, Text.pack prog)
    _ -> die "bad arguments"

  langFileContents <- TIO.readFile languageFile
  Lang _name abstractSyntax concreteSyntax statics dynamics
    <- case runParser parseLang languageFile langFileContents of
      Left bad -> die (errorBundlePretty bad)
      Right good -> pure good

  putStrLn $ "concrete syntax:\n" ++ show concreteSyntax

  tmTokens <- case tokenizeConcrete (keywords concreteSyntax) prog of
    Left err   -> die $ errorBundlePretty err
    Right good -> pure good

  parsedTm <- case fullParses (concreteParser concreteSyntax) tmTokens of
    ([parsedTm], _)  -> pure parsedTm
    (parses, report) -> die $ unlines $
      [ "failed to parse term:"
      , show (length parses) ++ " parses found:"
      , show parses
      , show report
      ]

  -- TODO: typecheck!
  -- runCheck (Env statics Map.empty) (check parsedTm)

  let mCore = runReaderT (termToCore parsedTm)
        (dynamics, abstractSyntax, _startSort abstractSyntax)

  core <- case mCore of
    Nothing   -> die "failed to translate term to core"
    Just core -> pure core

  val <- case evalCore core of
    Left msg  -> die msg
    Right val -> pure val

  valTm <- case valToTerm val of
    Nothing -> die "failed to translate value to term"
    Just tm -> pure tm

  putDoc $ runReader (prettyTm valTm) (0, concreteSyntax) <> hardline
