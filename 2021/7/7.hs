import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Data.Ix (range)
import Data.Maybe (fromMaybe)
import Safe

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

solve1 :: [Int] -> Int
solve1 xs = fromMaybe 0
          $ minimumMay
          -- the best position will be one of the existing positions
          -- so check the cost of moving to each existing position
          $ map (\x -> sum [abs (x - y) | y <- xs]) xs

solve2 :: [Int] -> Int
solve2 xs = fromMaybe 0
          $ minimumMay
          -- fuck it iterate through all possibilities
          $ map (\x -> sum [cost (abs (x - y)) | y <- xs])
          $ maybe [] range
          -- possible best position is somewhere in [min,max]
          $ liftA2 (,) (minimumMay xs) (maximumMay xs)
  where
    cost n = n * (n + 1) `div` 2

readInput :: String -> [Int]
readInput = map read . splitOn ','

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
