module Linguist.Languages.ModernizedAlgol where

import           Control.Lens
import           Data.Diverse
import           Data.Text                          (Text)
import           Data.Void                          (Void, absurd)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec                    (ParseError, runParser)

import           Linguist.Languages.Document.Syntax
import           Linguist.ParseSyntaxDescription
import           Linguist.Types                     (SyntaxChart, Term (..))

-- TODO:
-- "the expressions include those of PCF, augmented with one construct"
-- build pcf, formally relate these languages
syntaxText :: Text
syntaxText =
  [text|
Typ ::= Cmd

// pure expressions, whose meaning does not depend on any assignables
Exp ::= Cmd(Cmd)

// impure commands, whose meaning is given in terms of assignables
Cmd ::=
  Ret(Exp)
  Bnd(Exp; Exp. Cmd)
  Dcl(Exp; Exp. Cmd)
  Get[a]
  Set([a]; Exp)
  |]

$(do
  Just t <- lookupTypeName "Text"
  mkTypes syntaxText $ Map.fromList
    [
    ])
