module Test.ParseLanguage where

import           EasyTest

import Lvca.ParseLanguage

parseTest' :: ConcreteSyntaxDescriptionParser a -> Text -> PropertyT IO ()
parseTest' p input = case runParser (p <* scn <* eof) "test" input of
  Left err       -> crash $ errorBundlePretty err
  Right rules'   -> success

parseLanguageTests :: Test
parseLanguageTests = scope "parse-language" $ tests
  [ example $
    bracket
      (Text.readFile "../../languages/simple.lvca")
      (\_ -> pure ())
      (parseTest' syntaxDescription)
  ]
