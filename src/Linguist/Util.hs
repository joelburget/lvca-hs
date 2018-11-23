module Linguist.Util where

import           Control.Lens         (FunctorWithIndex, imap)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (StateT (..))
import           GHC.Stack

-- | Like 'zip', but lengths must match
pair :: [a] -> [b] -> Maybe [(a, b)]
pair [] []         = Just []
pair (a:as) (b:bs) = ((a, b):) <$> pair as bs
pair _ _           = Nothing

-- | Like 'zipWith', but lengths must match
pairWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pairWith _f []     []     = Just []
pairWith f  (a:as) (b:bs) = (f a b :) <$> pairWith f as bs
pairWith _ _ _            = Nothing

-- | Like indexed 'zipWith', but lengths must match
ipairWith :: (Int -> a -> b -> c) -> [a] -> [b] -> Maybe [c]
ipairWith = ipairWith' 0
  where
  ipairWith' _ _f []     []     = Just []
  ipairWith' i f  (a:as) (b:bs) = (f i a b :) <$> ipairWith' (succ i) f as bs
  ipairWith' _ _ _ _            = Nothing

mapAccumM
  :: (Monad m, Functor m, Traversable t)
  => (a -> b -> m (c, a))
  -> a
  -> t b
  -> m (t c, a)
mapAccumM f = flip (runStateT . (traverse (StateT . (flip f))))

infix 0 ??

(??) :: MonadError e m => Maybe a -> e -> m a
(Just a) ?? _  = pure a
Nothing ?? err = throwError err

infixl 1 <@&>
(<@&>) :: FunctorWithIndex i f => f a -> (i -> a -> b) -> f b
(<@&>) = flip imap

forceRight :: (HasCallStack, Show e) => Either e a -> a
forceRight (Right x) = x
forceRight (Left e) = error $ "forceRight: unexpectedly called with " ++ show e
