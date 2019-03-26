{-# language QuasiQuotes #-}
module Test.ParseBidirectional where

import Control.Lens
import           Data.Text                             (Text)
import           EasyTest                              -- (Test, property, (===))
import           Text.Megaparsec
  (errorBundlePretty, runParser)
import           NeatInterpolation

import           Lvca.Bidirectional
import           Lvca.ParseBidirectional
import qualified Test.Bidirectional as Bidirectional

testFile :: Text
testFile = [text|
----------------------- (bool intro 1)
ctx >> true() => bool()

------------------------ (bool intro 2)
ctx |- false() => bool()

//  ctx[x -> ty]
// -------------- (var)
// ctx |- x => ty

      ctx |- tm <= ty
-------------------------- (annot)
ctx |- annot(tm; ty) => ty

ctx |- tm1 <= bool()  ctx |- tm2 <= ty  ctx |- tm3 <= ty
-------------------------------------------------------- (bool elim)
            ctx |- ite(tm1; tm2; tm3) <= ty

    ctx, x : ty1 |- tm <= ty2
---------------------------------- (lam intro)
ctx |- lam(x. tm) <= arr(ty1; ty2)

ctx |- tm1 => arr(ty1; ty2)  ctx |- tm2 <= ty1
---------------------------------------------- (lam elim)
        ctx |- app(tm1; tm2) => ty2

ctx |- tm => ty
--- (switch)
ctx |- tm <= ty
  |]

testParseBidirectional :: Test
testParseBidirectional = example $
  case runParser parseBidirectional "(test)" testFile of
    Left err       -> crash $ errorBundlePretty err
    Right rules'   ->
      (rules' & traverse . name .~ Nothing) -- erase all names
      ===
      _rules Bidirectional.env
