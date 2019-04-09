module Lvca.Printer where

import           Control.Lens              hiding (Empty, mapping, op, prism)
import           Control.Monad.Reader
import           Data.List                 (find)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, isJust)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc hiding (space)
import           Data.Void                 (Void)
import           Prelude                   hiding (lookup)

import           Lvca.Types

type Printer = Reader (Int, ConcreteSyntax) (Doc ())

-- | Pretty-print a term given its conrete syntax chart.
prettyTm :: Term Void -> Printer
prettyTm tm = do
  (envPrec, ConcreteSyntax directives) <- ask
  case tm of
    Term name subtms -> fromMaybe

      -- If thrown this error is from one of the steps that uses `sameName`
      -- TODO: update
      (error $ "couldn't find " ++ show name ++
        " in concrete syntax directives")

      $ do

      -- Does this name, directive pair have the name we're looking for?
      let sameName (ConcreteSyntaxRule name' _ _) = name' == name

      ConcreteSyntaxRule _ slots directive
        <- findOf (traverse . traverse) sameName directives

      -- Find the precedence (determined by index) of the operator
      reverseOpPrec
        <- Seq.findIndexL (isJust . find sameName) directives

      -- reverseOperatorPrec counts from front to to back ([0..n]). But we want
      -- the first operators to have the highest precendence, so we reverse the
      -- precedence to [n..0].
      let opPrec = Seq.length directives - reverseOpPrec

          subtms' = subtms <&> \case
            Scope [] tm' -> tm'
            _            -> error "unsupported binding in binary operator"

          body = local (_1 .~ opPrec) $ case directive of
            MixfixDirective directive' -> prettyMixfix $ PrintInfo directive' $
              Map.fromList $ zipWith
                (\(binderNames, subtmName) subtm -> (subtmName, (binderNames, subtm)))
                slots
                subtms
            InfixDirective str fixity -> prettyInfix str fixity subtms'
            AssocDirective assoc      -> prettyAssoc assoc subtms'

      -- If the child has a lower precedence than the parent you must
      -- parenthesize it
      pure $ if opPrec <= envPrec then parens <$> body else body

    Var name    -> pure $ pretty name
    PrimValue a -> pure $ pretty a

data PrintInfo = PrintInfo
  !MixfixDirective
  !(Map Text ([Text], Scope Void))

prettyMixfix :: PrintInfo -> Printer
prettyMixfix = \case
  PrintInfo (Literal str)  _      -> pure $ pretty str

  PrintInfo (Sequence a b) m -> (<+>)
    <$> prettyMixfix (PrintInfo a m)
    <*> prettyMixfix (PrintInfo b m)

  PrintInfo Line          _ -> pure line
  PrintInfo (Nest i a)    m -> nest i <$> prettyMixfix (PrintInfo a m)
  PrintInfo (Group a)     m -> group <$> prettyMixfix (PrintInfo a m)
  PrintInfo (a :<+ _)     m -> prettyMixfix $ PrintInfo a m

  PrintInfo (VarName absName) m -> pure $ pretty absName -- XXX convert name
  PrintInfo (SubTerm sym) m -> case m ^?! ix sym of
    (_, Scope _ tm) -> prettyTm tm

prettyInfix :: Text -> Fixity -> [Term Void] -> Printer
prettyInfix str fixity subtms = case subtms of
  [l, r] -> do
    -- For infix operators, we call `pred` on the side that *should not* show a
    -- paren in the case of an operator of the same precedence.
    let assoc = local (_1 %~ pred)
        ret l' r' = mconcat [ l', " ", pretty str, " ", r' ]

    case fixity of
      Infix  -> ret <$> prettyTm l         <*> prettyTm r
      Infixl -> ret <$> assoc (prettyTm l) <*> prettyTm r
      Infixr -> ret <$> prettyTm l         <*> assoc (prettyTm r)
  _ -> error "expected two subterms for infix directive"

prettyAssoc :: Associativity -> [Term Void] -> Printer
prettyAssoc assoc = \case
  [l, r] -> do
    -- We call `pred` on the side that *should not* show a paren in the case of
    -- an operator of the same precedence.
    let assocPred = local (_1 %~ pred)
        ret l' r' = mconcat [ l', " ", r' ]
    case assoc of
      Assocl -> ret <$> assocPred (prettyTm l) <*> prettyTm r
      Assocr -> ret <$> prettyTm l <*> assocPred (prettyTm r)
  _ -> error "expected two subterms for assoc directive"