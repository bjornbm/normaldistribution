{- |
   Copyright  : Copyright (C) 2011 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : Stable
   Portability: Haskell 98

Zero-dependency normally distributed random numbers.

This purpose of this library is to have a simple API and no
dependencies beyond Haskell 98 in order to let you produce normally
distributed random values as soon as possible withe a minimum of
fuss. This library does not attempt to be blazingly fast nor to
pass stringent tests of randomness. It attempts to be very easy to
use while being good enough for many applications (simulations,
games...). The API is largely analogous to a subset of "System.Random".
Example use:

Pure:

> (sample,g) = normal  randomGen   -- using a 'System.Random.RandomGen'
> samples    = normals randomGen   -- infinite list
> samples2   = mkNormals 108314    -- infinite list using a seed

With custom mean and standard deviation, e.g.:

> sample'  = normal'  (mean,sigma) stdGen
> samples' = normals' (mean,sigma) stdGen

In the IO monad:

> sample   <- ioNormal
> samples  <- ioNormals
> sample'  <- ioNormal'  (mean,sigma)
> samples' <- ioNormals' (mean,sigma)

The library builds upon "System.Random" and uses the Central Limit
Theorem to approximate normally distributed values from multiple
uniformly distributed random values.

-}


module Data.Random.Normal
  ( normal
  , normals
  , mkNormals
  , normalIO
  , normalsIO

  , normal'
  , normals'
  , mkNormals'
  , normalIO'
  , normalsIO'

  ) where

import Data.List (mapAccumL)
import System.Random


-- Normal distribution approximation
-- ---------------------------------
-- | Central limit theorem for approximating normally distributed
-- sampling. Takes a list of no less than twelve random uniformly
-- distributed samples in the range [0,1] and uses the first twelve
-- samples to approximate a normally distributed random sample with
-- mean 0 and standard deviation 1.
centralLimitTheorem :: Fractional a => [a] -> a
centralLimitTheorem ss = sum (take 12 ss) - 6


-- API
-- ---
-- | Takes a random number generator g, and returns a random value
-- normally distributed with mean 0 and standard deviation 1,
-- together with a new generator. This function is ananalogous to
-- 'System.Random.random'.
normal :: (RandomGen g, Random a, Fractional a) => g -> (a,g)
normal g = (centralLimitTheorem as, g')
  -- While The Haskell 98 report says "For fractional types, the
  -- range is normally the semi-closed interval [0,1)" we will
  -- specify the range explicitely just to be sure.
  where (g',as) = iterateN 12 (swap . randomR (0,1)) g

-- | Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is ananalogous to 'System.Random.randoms'.
normals :: (RandomGen g, Random a, Fractional a) => g -> [a]
normals g = x:normals g' where (x,g') = normal g

-- | Creates a infinite list of normally distributed random values
-- from the provided random generator seed. (In the implementation
-- the seed is fed to 'Data.Random.mkStdGen' to produce the random
-- number generator.)
mkNormals :: (Random a, Fractional a) => Int -> [a]
mkNormals = normals . mkStdGen

-- | A variant of 'normal' that uses the global random number
-- generator.
normalIO :: (Random a, Fractional a) => IO a
normalIO = fmap centralLimitTheorem $ mapM randomRIO $ repeat (0,1)

-- | Creates a infinite list of normally distributed random values
-- using the global random number generator. (In the implementation
-- 'System.Random.newStdGen' is used.)
normalsIO :: (Random a, Fractional a) => IO [a]
normalsIO = fmap normals newStdGen


-- With mean and standard deviation
-- --------------------------------
-- | Analogous to 'normal' but uses the provided mean and standard
-- deviation.
normal' :: (RandomGen g, Random a, Fractional a) => (a,a) -> g -> (a,g)
normal' (mean, sigma) g = (x * sigma + mean, g') where (x, g') = normal g

-- | Analogous to 'normals' but uses the provided mean and standard
-- deviation.
normals' :: (RandomGen g, Random a, Fractional a) => (a,a) -> g -> [a]
normals' (mean, sigma) g = map (\x -> x * sigma + mean) (normals g)

-- | Analogous to 'mkNormals' but uses the provided mean and standard
-- deviation.
mkNormals' :: (Random a, Fractional a) => (a,a) -> Int -> [a]
mkNormals' ms= normals' ms . mkStdGen

normalIO' ::(Random a, Fractional a) => (a,a) -> IO a
normalIO' (mean,sigma) = fmap (\x -> x * sigma + mean) normalIO

-- | Creates a infinite list of normally distributed random values
-- using the global random number generator. (In the implementation
-- 'System.Random.newStdGen' is used.)
normalsIO' :: (Random a, Fractional a) => (a,a) -> IO [a]
normalsIO' ms = fmap (normals' ms) newStdGen


-- Helpers
-- -------
-- | Swap the elements in a tuple.
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- | Iterate on the accumulator a specified number of times.
iterateN :: Int -> (acc -> (acc, x)) -> acc -> (acc, [x])
iterateN n f a0 = mapAccumL (\a _ -> f a) a0 [1..n]
