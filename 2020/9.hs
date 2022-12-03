import qualified Data.Set as Set

doesSum :: [Int] -> Int -> Bool
doesSum xs a = any (\x -> Set.member (a - x) ss) xs
  where ss = Set.fromList xs

solve1' :: [Int] -> [Int] -> Int
solve1' _ [] = undefined
solve1' pre (x:xs) = if doesSum pre x
                     then solve1' (tail pre ++ [x]) xs
                     else x

solve1 :: [Int] -> Int
solve1 xs = solve1' (take 25 xs) (drop 25 xs)

getRange' :: Int -> Int -> [Int] -> [Int]
getRange' t c xs = case compare (sum (take c xs)) t of
                     EQ -> take c xs
                     LT -> getRange' t (c + 1) xs
                     GT -> getRange' t (c - 1) (tail xs)

getRange :: Int -> [Int] -> [Int]
getRange t = getRange' t 0

solve2 :: [Int] -> Int
solve2 xs = minimum rs + maximum rs
  where t = solve1 xs
        rs = getRange t xs

main :: IO ()
--main = interact (show . solve1 . map read . lines)
main = interact (show . solve2 . map read . lines)
