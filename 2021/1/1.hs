import Control.Arrow ((&&&))

-- |Get the sums of elements in the list using a sliding window of size `n`
sumWindow :: Num a => Int -> [a] -> [a]
sumWindow n xs = case n `compare` 1 of
  -- If the window is of zero/negative size, there's no result
  LT -> []
  -- A window of size one means summing one value, so the input is unchanged
  EQ -> xs
  -- Recurse to add each number to its adjancent window of one smaller size
  GT -> zipWith (+) xs $ sumWindow (n - 1) (tail xs)

solve1 :: [Int] -> Int
solve1 xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

solve2 :: [Int] -> Int
solve2 = solve1 . sumWindow 3

main :: IO ()
main = interact (show . (solve1 &&& solve2) . map read . lines)
