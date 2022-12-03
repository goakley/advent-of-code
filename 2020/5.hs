import Data.List (sort)

walk :: Int -> [Bool] -> Int
walk n = fst . foldl (\(l,h) x -> let d = (h - l) `div` 2 in if x then (l,l+d) else (l+d+1,h)) (0,n)

seat :: String -> Int
seat xs = a * 8 + b
  where a = walk 127 $ map (=='F') $ takeWhile (`elem` "FB") xs
        b = walk 7 $ map (=='L') $ dropWhile (`elem` "FB") xs

solve1 :: [String] -> Int
solve1 = maximum . map seat

solve2 :: [String] -> Int
solve2 xs = fst $ head $ filter (uncurry (/=)) pairs
  where seats = sort $ map seat xs
        pairs = zip [(head seats)..(last seats)] seats

main :: IO ()
--main = interact (show . solve1 . lines)
main = interact (show . solve2 . lines)
