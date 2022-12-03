import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as Map

-- convenience factor +1
type Map = Map.HashMap

-- |Get all the points adjacent to a given point
adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- |Determine if a point is a "lowest" point (surrounded by higher points)
isLowest :: Map (Int, Int) Int -> (Int, Int) -> Bool
isLowest m xy = case Map.lookup xy m of
  Nothing -> False
  Just v -> all (>v) $ mapMaybe (`Map.lookup` m) $ adjacent xy

-- |Get all the lowest points from a map (in no particular order)
lowestPoints :: Map (Int, Int) Int -> [((Int, Int), Int)]
lowestPoints m = filter (isLowest m . fst) $ Map.toList m

-- |Remove all the elements of the basin containing the specified point
-- and provide how many elements were in the basin
--
-- Points of height 9 or greater are not part of basins - they will be removed
-- from the map, but will contribute a size of 0.
removeBasin :: (Int, Int) -> Int -> Map (Int, Int) Int -> (Int, Map (Int, Int) Int)
removeBasin xy i m = case Map.lookup xy m of
  Nothing -> (i,m)
  Just v -> if v < 9
            then foldr (uncurry . removeBasin) (i+1, Map.delete xy m) $ adjacent xy
            else (i,Map.delete xy m)

-- |Get the sizes of all the basins in the map (in no particular order)
getBasinSizes :: Map (Int, Int) Int -> [Int]
getBasinSizes im = case Map.keys im of
  [] -> []
  (k:_) -> let (i, m) = removeBasin k 0 im
           in (if i > 0 then (i:) else id) $ getBasinSizes m

solve1 :: Map (Int, Int) Int -> Int
solve1 = sum . map ((+1) . snd) . lowestPoints

solve2 :: Map (Int, Int) Int -> Int
solve2 = product . take 3 . reverse . sort . getBasinSizes

readInput :: String -> Map (Int, Int) Int
readInput = Map.fromList
          . concat
          . zipWith (\x -> zipWith (\y c -> ((x,y),read [c])) [1..]) [1..]
          . lines

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
