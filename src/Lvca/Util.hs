module Lvca.Util where

import           Control.Lens
  (FunctorWithIndex, Prism', imap, preview, prism', review)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (StateT(..))
import           Data.Foldable        (asum)
import           Data.Matchable
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Stack

import           Lvca.FunctorUtil

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

mapAccumM
  :: (Monad m, Functor m, Traversable t)
  => (a -> b -> m (c, a))
  -> a
  -> t b
  -> m (t c, a)
mapAccumM f = flip (runStateT . (traverse (StateT . (flip f))))

-- | Lift a 'Maybe' to 'm'
(??) :: MonadError e m => Maybe a -> e -> m a
(Just a) ?? _   = pure a
Nothing  ?? err = throwError err
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

show1 :: (Show1 f, Show a) => f a -> String
show1 a = showsPrec1 0 a ""

show2 :: (Show2 f, Show a, Show b) => f a b -> String
show2 a = showsPrec2 0 a ""

tShow :: Show a => a -> Text
tShow = Text.pack . show

concat3 :: [[[[a]]]] -> [a]
concat3 = concat . concat . concat

sumPrisms
  :: Prism' a (f1 (Fix (f1 :+: f2)))
  -> Prism' a (f2 (Fix (f1 :+: f2)))
  -> Prism' a (Fix (f1 :+: f2))
sumPrisms p1 p2 = prism' rtl ltr where
  rtl = \case
    Fix (InL pat) -> review p1 pat
    Fix (InR pat) -> review p2 pat
  ltr tm = asum
    [ Fix . InL <$> preview p1 tm
    , Fix . InR <$> preview p2 tm
    ]

sumPrisms2'
  :: forall a f f1 f2.
     (f ~ Fix (f1 :+: f2))
  => (Prism' a f -> Prism' a (f1 f))
  -> (Prism' a f -> Prism' a (f2 f))
  -> Prism' a f
sumPrisms2' p1 p2 =
  let p :: Prism' a f
      p = sumPrisms (p1 p) (p2 p)
  in p

sumPrisms3
  :: Prism' a (f1 (Fix (f1 :+: f2 :+: f3)))
  -> Prism' a (f2 (Fix (f1 :+: f2 :+: f3)))
  -> Prism' a (f3 (Fix (f1 :+: f2 :+: f3)))
  -> Prism' a (Fix (f1 :+: f2 :+: f3))
sumPrisms3 p1 p2 p3 = prism' rtl ltr where
  rtl = \case
    Fix (InL pat)       -> review p1 pat
    Fix (InR (InL pat)) -> review p2 pat
    Fix (InR (InR pat)) -> review p3 pat
  ltr tm = asum
    [ Fix . InL       <$> preview p1 tm
    , Fix . InR . InL <$> preview p2 tm
    , Fix . InR . InR <$> preview p3 tm
    ]

sumPrisms3'
  :: forall a f f1 f2 f3.
     (f ~ Fix (f1 :+: f2 :+: f3))
  => (Prism' a f -> Prism' a (f1 f))
  -> (Prism' a f -> Prism' a (f2 f))
  -> (Prism' a f -> Prism' a (f3 f))
  -> Prism' a f
sumPrisms3' p1 p2 p3 =
  let p :: Prism' a f
      p = sumPrisms3 (p1 p) (p2 p) (p3 p)
  in p

sumPrisms4
  :: Prism' a (f1 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  -> Prism' a (f2 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  -> Prism' a (f3 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  -> Prism' a (f4 (Fix (f1 :+: f2 :+: f3 :+: f4)))
  -> Prism' a (Fix (f1 :+: f2 :+: f3 :+: f4))
sumPrisms4 p1 p2 p3 p4 = prism' rtl ltr where
  rtl = \case
    Fix (InL pat)             -> review p1 pat
    Fix (InR (InL pat))       -> review p2 pat
    Fix (InR (InR (InL pat))) -> review p3 pat
    Fix (InR (InR (InR pat))) -> review p4 pat
  ltr tm = asum
    [ Fix . InL             <$> preview p1 tm
    , Fix . InR . InL       <$> preview p2 tm
    , Fix . InR . InR . InL <$> preview p3 tm
    , Fix . InR . InR . InR <$> preview p4 tm
    ]

sumPrisms4'
  :: forall a f f1 f2 f3 f4.
     (f ~ Fix (f1 :+: f2 :+: f3 :+: f4))
  => (Prism' a f -> Prism' a (f1 f))
  -> (Prism' a f -> Prism' a (f2 f))
  -> (Prism' a f -> Prism' a (f3 f))
  -> (Prism' a f -> Prism' a (f4 f))
  -> Prism' a f
sumPrisms4' p1 p2 p3 p4 =
  let p :: Prism' a f
      p = sumPrisms4 (p1 p) (p2 p) (p3 p) (p4 p)
  in p
