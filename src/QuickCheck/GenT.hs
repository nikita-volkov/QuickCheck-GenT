{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Most of the code is borrowed from 
-- <http://haskell.1045720.n5.nabble.com/darcs-patch-GenT-monad-transformer-variant-of-Gen-QuickCheck-2-td3172136.html a mailing list discussion>.
-- Therefor, credits go to Paul Johnson and Felix Martini.
module QuickCheck.GenT
  ( GenT
  , runGenT
  , MonadGen(..)
  -- * Lifted functions
  , arbitrary
  , oneof
  , frequency
  , elements
  , growingElements
  , getSize
  , scale
  , suchThat
  , suchThatMap
  , suchThatMaybe
  , applyArbitrary2
  , applyArbitrary3
  , applyArbitrary4
  , listOf
  , listOf1
  , vectorOf
  , vector
  , infiniteListOf
  , infiniteList
  , shuffle
  , sublistOf
  , orderedList
  -- * Re-exports
  , QC.Gen
  -- * Safe functions
  , oneofMay
  , elementsMay
  , growingElementsMay
  ) where

import QuickCheck.GenT.Prelude
import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck.Arbitrary as QC
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC
import qualified System.Random as Random


newtype GenT m a = GenT { unGenT :: QC.QCGen -> Int -> m a }

instance (Functor m) => Functor (GenT m) where
  fmap f m = GenT $ \r n -> fmap f $ unGenT m r n

instance (Monad m) => Monad (GenT m) where
  return a = GenT (\_ _ -> return a)
  m >>= k = GenT $ \r n -> do
    let (r1, r2) = Random.split r
    a <- unGenT m r1 n
    unGenT (k a) r2 n

#if MIN_VERSION_base(4,13,0)
instance (MonadFail m) => MonadFail (GenT m) where
#endif
  fail msg = GenT (\_ _ -> fail msg)

instance (Functor m, Monad m) => Applicative (GenT m) where
  pure = return
  (<*>) = ap

instance MonadTrans GenT where
  lift m = GenT (\_ _ -> m)

instance (MonadIO m) => MonadIO (GenT m) where
  liftIO = lift . liftIO

runGenT :: GenT m a -> QC.Gen (m a)
runGenT (GenT run) = QC.MkGen run

class (Applicative g, Monad g) => MonadGen g where 
  liftGen :: QC.Gen a -> g a 
  variant :: Integral n => n -> g a -> g a 
  sized :: (Int -> g a) -> g a 
  resize :: Int -> g a -> g a 
  choose :: Random.Random a => (a, a) -> g a 

instance (Applicative m, Monad m) => MonadGen (GenT m) where
  liftGen gen = GenT $ \r n -> return $ QC.unGen gen r n
  choose rng = GenT $ \r _ -> return $ fst $ Random.randomR rng r
  variant k (GenT g) = GenT $ \r n -> g (var k r) n 
  sized f = GenT $ \r n -> let GenT g = f n in g r n 
  resize n (GenT g) = GenT $ \r _ -> g r n

instance MonadGen QC.Gen where 
  liftGen = id 
  variant k (QC.MkGen g) = QC.MkGen $ \r n -> g (var k r) n 
  sized f = QC.MkGen $ \r n -> let QC.MkGen g = f n in g r n 
  resize n (QC.MkGen g) = QC.MkGen $ \r _ -> g r n 
  choose range = QC.MkGen $ \r _ -> fst $ Random.randomR range r 

-- |
-- Private variant-generating function.  Converts an integer into a chain 
-- of (fst . split) and (snd . split) applications.  Every integer (including 
-- negative ones) will give rise to a different random number generator in 
-- log2 n steps. 
var :: Integral n => n -> QC.QCGen -> QC.QCGen
var k = 
  (if k == k' then id else var k') . (if even k then fst else snd) . Random.split 
  where k' = k `div` 2 

--------------------------------------------------------------------------
-- ** Lifted functions

arbitrary :: (Arbitrary a, MonadGen m) => m a
arbitrary = liftGen arbitrary

getSize :: MonadGen m => m Int
getSize = liftGen getSize

scale :: MonadGen m => (Int -> Int) -> m a -> m a
scale f g = sized (\n -> resize (f n) g)

applyArbitrary2 :: MonadGen m => (Arbitrary a, Arbitrary b) => (a -> b -> r) -> m r
applyArbitrary2 = liftGen . QC.applyArbitrary2

applyArbitrary3 :: MonadGen m => (Arbitrary a, Arbitrary b, Arbitrary c) => (a -> b -> c -> r) -> m r
applyArbitrary3 = liftGen . QC.applyArbitrary3

applyArbitrary4 :: MonadGen m => (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => (a -> b -> c -> d -> r) -> m r
applyArbitrary4 = liftGen . QC.applyArbitrary4

infiniteListOf :: MonadGen m => m a -> m [a]
infiniteListOf = sequence . repeat

infiniteList :: (Arbitrary a, MonadGen m) => m [a]
infiniteList = infiniteListOf arbitrary

shuffle :: MonadGen m => [a] -> m [a]
shuffle = liftGen . QC.shuffle

sublistOf :: MonadGen m => [a] -> m [a]
sublistOf = liftGen . QC.sublistOf

orderedList :: (Ord a, Arbitrary a, MonadGen m) => m [a]
orderedList = liftGen QC.orderedList

--------------------------------------------------------------------------
-- ** Common generator combinators

-- | Generates a value that satisfies a predicate.
suchThat :: MonadGen m => m a -> (a -> Bool) -> m a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

-- | Generates a value for which the given function returns a 'Just', and then
-- applies the function.
suchThatMap :: MonadGen m => m a -> (a -> Maybe b) -> m b
gen `suchThatMap` f =
  fmap fromJust $ fmap f gen `suchThat` isJust

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: MonadGen m => m a -> (a -> Bool) -> m (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1)
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)

-- | Generates a list of random length. The maximum length depends on the
-- size parameter.
listOf :: MonadGen m => m a -> m [a]
listOf gen = sized $ \n ->
  do k <- choose (0,n)
     vectorOf k gen

-- | Generates a non-empty list of random length. The maximum length
-- depends on the size parameter.
listOf1 :: MonadGen m => m a -> m [a]
listOf1 gen = sized $ \n ->
  do k <- choose (1,1 `max` n)
     vectorOf k gen

-- | Generates a list of the given length.
vectorOf :: MonadGen m => Int -> m a -> m [a]
vectorOf k gen = sequence [ gen | _ <- [1..k] ]

-- | Generates a list of a given length.
vector :: (Arbitrary a, MonadGen m) => Int -> m [a]
vector n = vectorOf n arbitrary


-- * Partial functions
-------------------------

-- | Randomly uses one of the given generators. The input list
-- must be non-empty.
oneof :: MonadGen m => [m a] -> m a
oneof = 
  fmap (fromMaybe (error "QuickCheck.GenT.oneof used with empty list")) .
  oneofMay

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency :: MonadGen m => [(Int, m a)] -> m a
frequency [] = error "QuickCheck.GenT.frequency used with empty list"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck.GenT.pick used with empty list"

-- | Generates one of the given values. The input list must be non-empty.
elements :: MonadGen m => [a] -> m a
elements = 
  fmap (fromMaybe (error "QuickCheck.GenT.elements used with empty list")) . 
  elementsMay

-- | Takes a list of elements of increasing size, and chooses
-- among an initial segment of the list. The size of this initial
-- segment increases with the size parameter.
-- The input list must be non-empty.
growingElements :: MonadGen m => [a] -> m a
growingElements =
  fmap (fromMaybe (error "QuickCheck.GenT.growingElements used with empty list")) .
  growingElementsMay


-- * Total functions resulting in Maybe
-------------------------

-- | 
-- Randomly uses one of the given generators.
oneofMay :: MonadGen m => [m a] -> m (Maybe a)
oneofMay = \case
  [] -> return Nothing
  l -> fmap Just $ choose (0, length l - 1) >>= (l !!)

-- | Generates one of the given values. 
elementsMay :: MonadGen m => [a] -> m (Maybe a)
elementsMay = \case
  [] -> return Nothing
  l -> Just . (l !!) <$> choose (0, length l - 1)

-- | Takes a list of elements of increasing size, and chooses
-- among an initial segment of the list. The size of this initial
-- segment increases with the size parameter.
growingElementsMay :: MonadGen m => [a] -> m (Maybe a)
growingElementsMay = \case
  [] -> return Nothing
  xs -> fmap Just $ sized $ \n -> elements (take (1 `max` size n) xs)
    where
      k = length xs
      mx = 100
      log' = round . log . fromIntegral
      size n = (log' n + 1) * k `div` log' mx
