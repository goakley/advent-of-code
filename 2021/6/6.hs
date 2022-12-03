import Control.Arrow ((&&&))
import qualified Data.Map as Map

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

-- |Count the number of elements in a list
count :: Ord a => [a] -> Map.Map a Int
count = foldr (Map.alter (Just . maybe (1::Int) (+1))) Map.empty

-- |A mapping from spawn age to the number of fish with that age
type Fishies = Map.Map Int Int

-- |Simulate one day of fish growth
grow :: Fishies -> Fishies
grow m = Map.insert 8 roe
       $ Map.mapKeysWith (+) (\k -> if k > 0 then k - 1 else 6) m
  where
    roe = Map.findWithDefault 0 0 m

-- |Get the number of fish after `n` days of fish growth
numberOfFish :: Int -> Fishies -> Int
numberOfFish n = sum . Map.elems . (!!n) . iterate grow

solve1 :: Fishies -> Int
solve1 = numberOfFish 80

solve2 :: Fishies -> Int
solve2 = numberOfFish 256

readInput :: String -> Fishies
readInput = count . map read . splitOn ','

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
