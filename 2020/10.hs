import Data.List (sort, tails)
import qualified Data.Map as Map

solve1 :: [Int] -> Int
solve1 xs = length ones * length threes
  where ordered = sort (0 : (maximum xs + 3) : xs)
        diffs = zipWith (flip (-)) ordered (tail ordered)
        ones = filter (==1) diffs
        threes = filter (==3) diffs

combos :: [Int] -> Map.Map Int Int
combos input = foldr (\(x:xs) a -> Map.insert x (sum (map (a Map.!) xs)) a) (Map.singleton (length ordered - 1) 1) candidates
  where ordered = sort (0 : (maximum input + 3) : input)
        nexts = map (\(x:xs) -> map fst (x : takeWhile (\(_,v) -> v - snd x <= 3) xs)) $ init $ tails $ zip [0..] ordered
        candidates = init nexts

solve2 :: [Int] -> Int
solve2 xs = combos xs Map.! 0

main :: IO ()
--main = interact (show . solve1 . map read . lines)
main = interact (show . solve2 . map read . lines)
