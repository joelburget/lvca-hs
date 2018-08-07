module Linguist.Util where

pair :: [a] -> [b] -> Maybe [(a, b)]
pair [] []         = Just []
pair (a:as) (b:bs) = ((a, b):) <$> pair as bs
pair _ _           = Nothing

pairWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pairWith _f []     []     = Just []
pairWith f  (a:as) (b:bs) = (f a b :) <$> pairWith f as bs
pairWith _ _ _            = Nothing
