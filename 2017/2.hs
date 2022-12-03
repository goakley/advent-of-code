import Data.List (sort)
import Data.Maybe (listToMaybe)
import System.IO.Unsafe (unsafePerformIO)

input :: String
input = unsafePerformIO (readFile "2")

-- |Find the statistical range of some values
range :: [Int] -> Int
range xs = maximum xs - minimum xs

-- |Get the quotient of the first pair of evenly-divisible numbers
--
-- The input is assumed to be in sorted order.
firstDivisibleQuotient :: [Int] -> Int
firstDivisibleQuotient [] = undefined
firstDivisibleQuotient (x:xs) = maybe (firstDivisibleQuotient xs) (`div` x) $ listToMaybe $ filter ((==0) . (`mod` x)) xs

-- |Sum the result of a collapsing function applied to each row of the input
solve :: ([Int] -> Int) -> Int
solve f = sum
  $ map (f . map read . words)
  $ lines input

-- |Solve part 1 (the range checksum)
solve1 :: Int
solve1 = solve range

-- |Solve part 2 (the divisible quotient sum)
solve2 :: Int
solve2 = solve (firstDivisibleQuotient . sort)

main :: IO ()
main = print solve1 >> print solve2
