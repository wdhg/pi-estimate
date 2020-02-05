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

sample :: Int -> [Coord]
sample
  = randoms . mkStdGen

estimate :: Int -> Int -> Double
estimate n
  = estimate' . take n . sample

parEstimate :: Int -> Int -> Int -> Double
parEstimate threads n seed
  = estimate' points
    where
      g = mkStdGen seed
      points
        = concat $ parMap rseq (take n . sample) $ take threads $ randoms g

estimate' :: [Coord] -> Double
estimate' points
  = 4 * (fromIntegral count) / (fromIntegral $ length points)
    where
      count
        = length $ filter ((< 1) . magnitude) points

  {-
-- non-parallel
main :: IO ()
main
  = putStrLn $ show $ estimate $ sample 1000000 0
  -}

main :: IO ()
main
  = print $ parEstimate 12 1000000 0
