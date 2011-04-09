import Data.List (sort)
import Graphics.Rendering.Chart.Simple

import Data.Random.Normal


-- | The integral of the probability density from -inf to x for
-- the normal distribution with mean 0 and standard deviation 1.
-- This formula is an approximation taken from Beta.
normalDistribution :: Double -> Double
normalDistribution x = if x >= -100 then 1 / (1 + exp (-p x))
                 else 0 where p x = x * (1.59145 + 0.01095*x + 0.06651*x^2)

samples = mkNormals 165795 :: [Double]

xs n = sort $ take n samples
ys n = map (/m) [1..m] where m = fromIntegral n :: Double

n1 = 100
n2 = 10000

main = do
  plotWindow (xs n1) (ys n1) normalDistribution
  plotWindow (xs n2) (ys n2) normalDistribution
