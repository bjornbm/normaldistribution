{- |
   Copyright  : Copyright (C) 2011 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : Stable
   Portability: Haskell 98

This purpose of this library is to have a simple API and no
dependencies beyond Haskell 98 in order to let you produce normally
distributed random values with a minimum of fuss. This library does
/not/ attempt to be blazingly fast nor to pass stringent tests of
randomness. It attempts to be very easy to install and use while
being \"good enough\" for many applications (simulations, games, etc.).
The API builds upon and is largely analogous to that of the Haskell
98 @Random@ module (more recently @System.Random@).

Pure:

> (sample,g) = normal  myRandomGen  -- using a Random.RandomGen
> samples    = normals myRandomGen  -- infinite list
> samples2   = mkNormals 10831452   -- infinite list using a seed

In the IO monad:

> sample    <- normalIO
> samples   <- normalsIO  -- infinite list

With custom mean and standard deviation:

> (sample,g) = normal'    (mean,sigma) myRandomGen
> samples    = normals'   (mean,sigma) myRandomGen
> samples2   = mkNormals' (mean,sigma) 10831452

> sample    <- normalIO'  (mean,sigma)
> samples   <- normalsIO' (mean,sigma)

Internally the library uses the Central Limit Theorem to approximate
normally distributed values from multiple uniformly distributed
random values.

-}


module Data.Random.Normal (
  -- * Pure interface
    normal
  , normals
  , mkNormals

  -- ** Custom mean and standard deviation
  , normal'
  , normals'
  , mkNormals'

  -- * Using the global random number generator
  , normalIO
  , normalsIO

  -- ** Custom mean and standard deviation
  , normalIO'
  , normalsIO'

  ) where

import List (mapAccumL)  -- Data.List
import Random            -- System.Random


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
-- ===
-- | Takes a random number generator g, and returns a random value
-- normally distributed with mean 0 and standard deviation 1,
-- together with a new generator. This function is ananalogous to
-- 'Random.random'.
normal :: (RandomGen g, Random a, Fractional a) => g -> (a,g)
normal g = (centralLimitTheorem as, g')
  -- While The Haskell 98 report says "For fractional types, the
  -- range is normally the semi-closed interval [0,1)" we will
  -- specify the range explicitely just to be sure.
  where (g',as) = iterateN 12 (swap . randomR (0,1)) g

-- | Plural variant of 'normal', producing an infinite list of
-- random values instead of returning a new generator. This function
-- is ananalogous to 'Random.randoms'.
normals :: (RandomGen g, Random a, Fractional a) => g -> [a]
normals g = x:normals g' where (x,g') = normal g

-- | Creates a infinite list of normally distributed random values
-- from the provided random generator seed. (In the implementation
-- the seed is fed to 'Random.mkStdGen' to produce the random
-- number generator.)
mkNormals :: (Random a, Fractional a) => Int -> [a]
mkNormals = normals . mkStdGen


-- | A variant of 'normal' that uses the global random number
-- generator. This function is analogous to 'Random.randomIO'.
normalIO :: (Random a, Fractional a) => IO a
normalIO = fmap centralLimitTheorem $ mapM randomRIO $ repeat (0,1)

-- | Creates a infinite list of normally distributed random values
-- using the global random number generator. (In the implementation
-- 'Random.newStdGen' is used.)
normalsIO :: (Random a, Fractional a) => IO [a]
normalsIO = fmap normals newStdGen


-- With mean and standard deviation
-- --------------------------------
-- | Analogous to 'normal' but uses the supplied (mean, standard
-- deviation).
normal' :: (RandomGen g, Random a, Fractional a) => (a,a) -> g -> (a,g)
normal' (mean, sigma) g = (x * sigma + mean, g') where (x, g') = normal g

-- | Analogous to 'normals' but uses the supplied (mean, standard
-- deviation).
normals' :: (RandomGen g, Random a, Fractional a) => (a,a) -> g -> [a]
normals' (mean, sigma) g = map (\x -> x * sigma + mean) (normals g)

-- | Analogous to 'mkNormals' but uses the supplied (mean, standard
-- deviation).
mkNormals' :: (Random a, Fractional a) => (a,a) -> Int -> [a]
mkNormals' ms= normals' ms . mkStdGen


-- | Analogous to 'normalIO' but uses the supplied (mean, standard
-- deviation).
normalIO' ::(Random a, Fractional a) => (a,a) -> IO a
normalIO' (mean,sigma) = fmap (\x -> x * sigma + mean) normalIO

-- | Analogous to 'normalsIO' but uses the supplied (mean, standard
-- deviation).
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
