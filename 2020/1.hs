import Data.List (sort)
import Data.Maybe (mapMaybe, fromJust)

solve :: Int -> [Int] -> Maybe Int
solve i input = cur fwd rev
  where fwd = sort input
        rev = reverse fwd
        cmp = 2020 - i
        cur _ [] = Nothing
        cur [] _ = Nothing
        cur (a:as) (b:bs) = case compare (a + b) cmp of
                              LT -> cur as (b:bs)
                              GT -> cur (a:as) bs
                              EQ -> Just (a * b)

solve1 :: [Int] -> Int
solve1 = fromJust . solve 0

solve2 :: [Int] -> Int
solve2 input = head $ mapMaybe (\i -> (*i) <$> solve i input) input

main :: IO ()
--main = interact (show . solve1 . map read . lines)
main = interact (show . solve2 . map read . lines)
