import Control.Monad ((>=>))
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

estimate :: Int -> IO Double
estimate n
  = do
    g <- newStdGen
    let points = take n $ randoms g
        count  = length $ filter ((< 1.0) . magnitude) $ points
    return $ 4 * (fromIntegral count) / (fromIntegral n)

main :: IO ()
main
  = mapM_ (estimate >=> print) [100,200..10000]
