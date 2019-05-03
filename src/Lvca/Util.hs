module Lvca.Util where

import           Control.Lens         (FunctorWithIndex, imap)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (StateT(..))
import           Data.Matchable
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Stack

pattern (:->) :: a -> b -> (a, b)
pattern a :-> b = (a, b)

-- | Like 'zip', but lengths must match
pair :: [a] -> [b] -> Maybe [(a, b)]
pair = zipMatch

-- | Like 'zipWith', but lengths must match
pairWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pairWith f = zipMatchWith $ \a b -> Just $ f a b

-- | Like indexed 'zipWith', but lengths must match
ipairWith :: (Int -> a -> b -> c) -> [a] -> [b] -> Maybe [c]
ipairWith = ipairWith' 0
 where
  ipairWith' _ _f []     []     = Just []
  ipairWith' i f  (a:as) (b:bs) = (f i a b :) <$> ipairWith' (succ i) f as bs
  ipairWith' _ _  _      _      = Nothing

zipWithA :: Applicative p => (a -> b -> p c) -> [a] -> [b] -> p [c]
zipWithA f x y = sequenceA (zipWith f x y)

mapAccumM
  :: (Monad m, Functor m, Traversable t)
  => (a -> b -> m (c, a))
  -> a
  -> t b
  -> m (t c, a)
mapAccumM f = flip (runStateT . (traverse (StateT . (flip f))))

-- | Lift a 'Maybe' to 'm'
(??) :: MonadError e m => Maybe a -> e -> m a
Just a  ?? _   = pure a
Nothing ?? err = throwError err
infix 1 ??

-- | Lift an @m Maybe@ to @m@
(???) :: MonadError e m => m (Maybe a) -> e -> m a
m ??? err = do
  m' <- m
  m' ?? err
infix 1 ???

-- | Indexed form of '<&>'
(<@&>) :: FunctorWithIndex i f => f a -> (i -> a -> b) -> f b
(<@&>) = flip imap
infixl 1 <@&>

forceRight :: (HasCallStack, Show e) => Either e a -> a
forceRight (Right x) = x
forceRight (Left  e) = error $ "forceRight: unexpectedly called with " ++ show e

tShow :: Show a => a -> Text
tShow = Text.pack . show

concat3 :: [[[[a]]]] -> [a]
concat3 = concat . concat . concat
