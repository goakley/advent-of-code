import Text.Regex.TDFA ((=~))

rawInput :: String
rawInput = unlines [
  "Disc #1 has 17 positions; at time=0, it is at position 5.",
  "Disc #2 has 19 positions; at time=0, it is at position 8.",
  "Disc #3 has 7 positions; at time=0, it is at position 1.",
  "Disc #4 has 13 positions; at time=0, it is at position 7.",
  "Disc #5 has 5 positions; at time=0, it is at position 1.",
  "Disc #6 has 3 positions; at time=0, it is at position 0."]

readLine :: String -> (Int,Int)
readLine s = case m of
              [[_,i,n,o]] -> (read n,((read n - read o) - read i) `mod` read n)
              _ -> undefined
  where m = s =~ "^Disc #([0-9]+) has ([0-9]+) positions; at time=0, it is at position ([0-9]+)\\.$"

input :: [(Int,Int)]
input = map readLine $ lines rawInput

series :: [(Int,Int)] -> [[Int]]
series = map (\(n,o) -> map (\i -> o + (n * i)) [0..])

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys) = case compare x y of
                           EQ -> x : intersect xs ys
                           LT -> xs `intersect` (y:ys)
                           GT -> (x:xs) `intersect` ys

solve :: [[Int]] -> [Int]
solve [] = []
solve [xs] = xs
solve (xs:ys:zss) = solve $ intersect xs ys : zss

solution1 :: Int
solution1 = head $ solve $ series input

solution2 :: Int
solution2 = head $ solve $ series $ input ++ [readLine ("Disc #" ++ show (length input + 1) ++ " has 11 positions; at time=0, it is at position 0.")]

main :: IO ()
main = print solution1 >> print solution2
