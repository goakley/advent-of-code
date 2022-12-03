import Data.Map (Map, adjust, fromList, lookup)
import System.IO.Unsafe (unsafePerformIO)

input :: [Int]
input = map read $ lines $ unsafePerformIO (readFile "5")

commands :: Map Int Int
commands = Data.Map.fromList $ zip [0..] input

count :: Int -> Map Int Int -> (Int -> Int) -> Int
count p cs f = maybe 0 (\c -> 1 + count (p + c) (Data.Map.adjust (+(f c)) p cs) f) $ Data.Map.lookup p cs

steps :: (Int -> Int) -> Int
steps = count 0 commands 

solve1 :: Int
solve1 = steps (const 1)

solve2 :: Int
solve2 = steps (\c -> if c >= 3 then (-1) else 1)

main :: IO ()
main = print solve1 >> print solve2
