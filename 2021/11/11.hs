{-# LANGUAGE TupleSections #-}
import Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- convenience factor +2
type Map = Map.HashMap
type Set = Set.HashSet

-- |Get all the points adjacent to a given point (including itself)
adjacent :: (Int,Int) -> [(Int,Int)]
-- literally including itself because i'm too lazy to write code to exlucde itself
adjacent (x,y) = [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1]]

-- |Cause all of the eligible octopi to flash,
-- including those made eligible by others flashing,
-- and provide the number of octopi who flashed during the round
doFlash :: Set (Int,Int) -> Map (Int, Int) Int -> (Int, Map (Int, Int) Int)
doFlash flashed m = if Set.null flashed'
                    then (Set.size flashed,m)
                    else doFlash (Set.union flashed flashed') m'
  where (flashed', candidates) = Map.foldrWithKey (\xy v (fs,a) -> if v > 9 && not (Set.member xy flashed)
                                                                   then (Set.insert xy fs,adjacent xy ++ a)
                                                                   else (fs,a)
                                                  ) (Set.empty,[]) m
        m' = foldr (Map.adjust (+1)) m candidates

-- |Cause the ocotpi to process one round, counting how many of them flashed
update :: Map (Int, Int) Int -> (Int, Map (Int, Int) Int)
update m = fmap (Map.map (\v -> if v > 9 then 0 else v))
         $ doFlash Set.empty
         $ Map.map (+1) m

solve1 :: Map (Int, Int) Int -> Int
solve1 = sum . map fst . take 100 . tail . iterate (update . snd) . (0,)

solve2 :: Map (Int, Int) Int -> Int
solve2 m = let target = Map.size m
           in fst
            $ head
            $ filter ((==target) . snd)
            $ zip [1..]
            $ map fst
            $ tail
            $ iterate (update . snd) (0,m)

readInput :: String -> Map (Int, Int) Int
readInput = Map.fromList
          . concat
          . zipWith (\x -> zipWith (\y c -> ((x,y),read [c])) [1..]) [1..]
          . lines

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
