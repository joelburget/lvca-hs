{-# LANGUAGE QuasiQuotes         #-}
module Test.Document where

import           NeatInterpolation

import           Linguist.Util                      (forceRight)

textDocument :: Text
textDocument = [text|
# important document

*this* is the important document you've heard about. **never** mention it to anyone
|]

documentTests :: Test ()
documentTests = tests
  [ do Just doc  <- pure $ textDocument ^? foldText
       Just doc' <- pure $ doc          ^? foldTerm
       expectEq textDocument doc'

  , scope "parse" $ parseTest
      (ParseEnv (forceRight syntax) "Document" UntaggedExternals
        externalParsers)
      "Document(Cons(a; a))" $
      Fix $ Term "Document"
        [ Fix $ Term "Cons" [ Fix $ Var "a", Fix $ Var "a" ] ]

  , scope "prop_parse_pretty" $ testProperty $
    prop_parse_pretty (forceRight syntax) "Document"
      (const Nothing) externalParsers

  , scope "prop_serialise_identity" $ testProperty $
    prop_serialise_identity @() (forceRight syntax) "Document" (const Nothing)
  ]
