import Data.List (sort)
import Data.Set (fromList, size)
import System.IO.Unsafe (unsafePerformIO)

input :: [String]
input = lines $ unsafePerformIO (readFile "4")

-- |Get the number of valid passphrases given a word transformer
solve :: (String -> String) -> Int
solve f = length
  $ filter id
  $ zipWith (==) (map (length . words) input)
  $ map (Data.Set.size . Data.Set.fromList . map f . words) input

-- |Solve part 1 (phrases with no duplicate words)
solve1 :: Int
solve1 = solve id

-- |Solve part 2 (phrases with no shuffled duplicate words)
solve2 :: Int
solve2 = solve sort

main :: IO ()
main = print solve1 >> print solve2
