module Linguist.Util where

import Control.Monad.State

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

mapAccumM
  :: (Monad m, Functor m, Traversable t)
  => (a -> b -> m (c, a))
  -> a
  -> t b
  -> m (t c, a)
mapAccumM f = flip (runStateT . (traverse (StateT . (flip f))))
