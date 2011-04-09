import Data.Complex
import Control.Monad
import System.Random

import Data.Random.Normal
import Criterion.Main



gaussTwoAtATime :: Floating a =>  [a] -> [a]
gaussTwoAtATime (u1:u2:rest)
  = sqrt(-2*log(u1))*cos(2*pi*u2) : sqrt(-2*log(u1))*sin(2*pi*u2) : gaussTwoAtATime rest
gaussTwoAtATime _ = []

mkGauss :: (Random a, Floating a) => Int -> [a]
mkGauss = gaussTwoAtATime . randoms . mkStdGen

boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

boxMullerL :: Floating a => [a] -> [a]
boxMullerL (u1:u2:us) = n1:n2:boxMullerL us
  where (n1,n2) = boxMuller u1 u2
boxMullerL _ = []

mkBM :: (Random a, RealFloat a) => Int -> [a]
mkBM = boxMullerL . randoms . mkStdGen

seed = 14907417401

main = defaultMain
     [ bgroup "Double"  [ bench "gauss"   $ nf (take 1000 . mkGauss   :: Int -> [Double]) seed
                        , bench "BM"      $ nf (take 1000 . mkBM      :: Int -> [Double]) seed
                        , bench "CLT"     $ nf (take 1000 . mkNormals :: Int -> [Double]) seed
                        ]
     ]

{-
ghc --make -O2 Bench && ./Bench

warming up
estimating clock resolution...
mean is 19.24846 us (40001 iterations)
found 1405 outliers among 39999 samples (3.5%)
  1043 (2.6%) high severe
estimating cost of a clock call...
mean is 209.4200 ns (83 iterations)
found 8 outliers among 83 samples (9.6%)
  4 (4.8%) high mild
  4 (4.8%) high severe

benchmarking Double/gauss
collecting 100 samples, 7 iterations each, in estimated 2.032194 s
bootstrapping with 100000 resamples
mean: 2.926226 ms, lb 2.913109 ms, ub 2.980940 ms, ci 0.950
std dev: 118.4437 us, lb 21.64372 us, ub 277.3659 us, ci 0.950
found 10 outliers among 100 samples (10.0%)
  4 (4.0%) high mild
  6 (6.0%) high severe
variance introduced by outliers: 0.999%
variance is unaffected by outliers

benchmarking Double/BM
collecting 100 samples, 7 iterations each, in estimated 1.974073 s
bootstrapping with 100000 resamples
mean: 2.850987 ms, lb 2.840125 ms, ub 2.876605 ms, ci 0.950
std dev: 80.63213 us, lb 41.47637 us, ub 160.8647 us, ci 0.950
found 10 outliers among 100 samples (10.0%)
  3 (3.0%) high mild
  7 (7.0%) high severe
variance introduced by outliers: 0.998%
variance is unaffected by outliers

benchmarking Double/CLT
collecting 100 samples, 1 iterations each, in estimated 3.435516 s
bootstrapping with 100000 resamples
mean: 34.97446 ms, lb 34.73249 ms, ub 35.36789 ms, ci 0.950
std dev: 1.563835 ms, lb 1.103414 ms, ub 2.321097 ms, ci 0.950
found 13 outliers among 100 samples (13.0%)
  2 (2.0%) high mild
  11 (11.0%) high severe
variance introduced by outliers: 0.999%
variance is unaffected by outliers
-}
