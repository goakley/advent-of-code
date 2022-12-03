{-# LANGUAGE TupleSections #-}
import Control.Arrow ((&&&))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

-- |Group a a list into sublists of size `n`
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = uncurry (:) $ group n <$> splitAt n xs

type Board = (Map.Map Int (Int,Int), Set.Set (Int,Int))

-- |Determine if a board has bingo
isBingo :: Board -> Bool
isBingo (_, seen) = any (all (`Set.member` seen)) (rows ++ cols)
  where
    cols = map (zip [1..5] . repeat) [1..5]
    rows = map (flip zip [1..5] . repeat) [1..5]

-- |Add a called bingo number to the board state
addNumber :: Int -> Board -> Board
addNumber i (nums, seen) = maybe (nums, seen) (\v -> (nums, Set.insert v seen)) $ Map.lookup i nums

-- |Get the sum of all unmarked spots on a board
sumUnmarked :: Board -> Int
sumUnmarked (board, seen) = Map.foldrWithKey (\n xy a -> a + (if Set.member xy seen then 0 else n)) 0 board

-- |Get the boards that didn't have bingo (state one) but now have bingo (state two)
changeInBingo :: [Board] -> [Board] -> [Board]
changeInBingo xs = filter (\y -> any (\x -> fst x == fst y && not (isBingo x) && isBingo y) xs)

-- |Play bingo until all numbers are called, producing all intermediate states
playBingo :: [Int] -> [Board] -> [(Int,[Board])]
playBingo [] _ = []
playBingo (x:xs) boards = (x,boards') : playBingo xs boards'
  where boards' = map (addNumber x) boards

-- |Play bingo but provide the game states at which someone achieved bingo
-- (not optimised code but it works)
playBingoInflections :: [Int] -> [Board] -> [(Int,Board)]
playBingoInflections xs boards = concat $ zipWith (\(_,a) (i,b) -> map (i,) (changeInBingo a b)) results (tail results)
  where results = playBingo xs boards

solve1 :: ([Int], [Board]) -> Int
solve1 = maybe (error "unsolvable") (\(i,b) -> i * sumUnmarked b)
       . listToMaybe
       . uncurry playBingoInflections

solve2 :: ([Int], [Board]) -> Int
solve2 = maybe (error "unsolvable") (\(i,b) -> i * sumUnmarked b)
       . listToMaybe
       . reverse
       . uncurry playBingoInflections

readInput :: String -> ([Int], [Board])
readInput str = (numbers, zip boards (repeat Set.empty))
  where
    ls = lines str
    (h:xs) = ls
    numbers = map read $ splitOn ',' h
    -- shield your eyes
    boards = map (\(_:ys) -> Map.fromList $ concatMap (\(x,zs) -> zipWith (\y z -> (read z,(x,y))) [1..] zs) (zip [1..] ys)) $ group 6 (map words xs)

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
