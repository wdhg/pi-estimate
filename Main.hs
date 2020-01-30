import Control.Monad               ((>=>))
import Control.Parallel.Strategies
import System.Random

data Coord
  = Coord Double Double
    deriving
      Show

instance Bounded Coord where
  minBound = Coord 0.0 0.0
  maxBound = Coord 1.0 1.0

instance Random Coord where
  randomR (Coord lowerX lowerY, Coord upperX upperY) g
    = (Coord x y, g'')
      where
        (x, g')  = randomR (lowerX, upperX) g
        (y, g'') = randomR (lowerY, upperY) g'
  random g
    = randomR (minBound, maxBound) g

magnitude :: Coord -> Double
magnitude (Coord x y)
  = sqrt $ (x ^ 2) + (y ^ 2)

sample :: Int -> Int -> (Int, Int)
sample n seed
  = (count, length points)
    where
      g      = mkStdGen seed
      points = take n $ randoms g
      count  = length $ filter ((< 1.0) . magnitude) $ points

estimate :: Int -> Int -> Double
estimate n seed
  = 4 * (fromIntegral count) / (fromIntegral total)
    where
      (count, total)
        = sample n seed

join :: (Int, Int) -> (Int, Int) -> (Int, Int)
join (inside1, total1) (inside2, total2)
  = (inside1 + inside2, total1 + total2)

parEstimate :: [Int] -> Int -> Double
parEstimate ns seed
  = 4 * (fromIntegral count) / (fromIntegral total)
    where
      g = mkStdGen seed
      seeds = randoms g
      (count, total)
        = foldl1 join $ parMap rseq (uncurry sample) $ zip ns seeds

  {-
-- non-parallel
main :: IO ()
main
  = putStrLn $ show $ estimate $ sample 1000000 0
  -}

main :: IO ()
main
  = print $ parEstimate (replicate 12 100000) 0
