{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Data.Syntax.Earley
Description :  Syntax instance for Earley.Parser.
Copyright   :  (c) Paweł Nowak
License     :  MIT
Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental
Provides a Syntax instance for Attoparsec.Text.Parser.
-}
module Data.Syntax.Earley where

import           Control.Arrow (Kleisli(..))
import           Control.Category
import           Control.Category.Structures
import           Control.Lens.SemiIso
import           Control.Monad
import           Control.SIArrow
-- import           Data.Scientific
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Text (Text)
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as VU
import           Prelude hiding (id, (.))

-- import           Text.Earley

data ParserDesc a b where
  Id :: ParserDesc a a
  Compose :: ParserDesc b c -> ParserDesc a b -> ParserDesc a c
  (:***)  :: ParserDesc a b -> ParserDesc c d -> ParserDesc (a, c) (b, d)
  (:+++)  :: ParserDesc a b -> ParserDesc c d -> ParserDesc (Either a c) (Either b d)
  CEmpty  :: ParserDesc a b
  (:/+/)  :: ParserDesc a b -> ParserDesc a b -> ParserDesc a b
  SI      :: ReifiedSemiIso' a b -> ParserDesc a b
  SIBind  :: ReifiedSemiIso a (cat a b) (cat a b) b -> ParserDesc a b

newtype ReifiedSemiIso s t a b = ReifiedSemiIso { runSemiIso :: SemiIso s t a b }

instance Category ParserDesc where
  id = Id
  (.) = Compose

instance Products ParserDesc where
  (***) = (:***)

instance Coproducts ParserDesc where
  (+++) = (:+++)

instance CatPlus ParserDesc where
  cempty = CEmpty
  (/+/)  = (:/+/)

reifySemiIso' :: ASemiIso s t a b -> ReifiedSemiIso s t a b
reifySemiIso' ai = ReifiedSemiIso $ cloneSemiIso ai

instance SIArrow ParserDesc where
  siarr = SI . reifySemiIso
  sibind = SIBind . reifySemiIso'

instance Syntax ParserDesc where
  type Seq ParserDesc = Text
--     anyChar = wrap AP.anyChar
--     char = wrap . void . AP.char
--     notChar = wrap . AP.notChar
--     satisfy = wrap . AP.satisfy
--     string = wrap . void . AP.string
--     take = wrap . AP.take
--     takeWhile = wrap . AP.takeWhile
--     takeWhile1 = wrap . AP.takeWhile1
--     takeTill = wrap . AP.takeTill
--     vecN n f = wrap $ V.replicateM n $ unwrap f ()
--     ivecN n f = wrap $ V.generateM n $ fmap snd . unwrap f
--     uvecN n f = wrap $ VU.replicateM n $ unwrap f ()
--     uivecN n f = wrap $ VU.generateM n $ fmap snd . unwrap f

-- instance Isolable WrappedParser where
--     isolate p = Wrapped $ Kleisli $ either fail return . AP.parseOnly (unwrap p ())

-- instance SyntaxChar WrappedParser where
--     decimal = wrap AP.decimal
--     hexadecimal = wrap AP.hexadecimal
--     realFloat = wrap $ fmap toRealFloat AP.scientific
--     scientific = wrap AP.scientific

-- -- | Extracts the parser.
-- getParser :: WrappedParser a b -> a -> AP.Parser b
-- getParser (Wrapped (Kleisli f)) = f

-- -- | Extracts the parser.
-- getParser_ :: WrappedParser () b -> AP.Parser b
-- getParser_ (Wrapped (Kleisli f)) = f ()
