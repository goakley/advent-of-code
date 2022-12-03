import Debug.Trace (trace)
import Data.List (minimumBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

readInput :: String -> (Int,[Maybe Int])
readInput input = (read a, xs)
  where [a,b] = lines input
        xs = map (\x -> if x == "x" then Nothing else Just (read x)) $ words $ map (\i -> if i == ',' then ' ' else i) b

solve1 :: (Int,[Int]) -> Int
solve1 (k,xs) = waittime * bid
  where waittimes = map (\x -> (x - (k `mod` x))) xs
        (waittime,bid) = minimumBy (comparing fst) $ zip waittimes xs

solve2 :: [Maybe Int] -> Int
--solve2 ((Just x):xs) = head $ foldl (\as (i,n) -> filter (\a -> ((a+i) `mod` n) == 0) as) base pairs
--  where base = map (*x) [1..]
--        pairs = catMaybes $ zipWith (\i x -> maybe Nothing (\xx -> Just (i,xx)) x) [1..] xs
solve2 xs = head $ foldl (\as (i,n) -> trace (show (take 20 as))filter (\a -> ((a + i) `mod` n) == 0) as) (trace (show (take 4 base)) base) (trace (show pairs) pairs)
  where ((starti,startn):pairs) = reverse $ sortBy (comparing snd) $ catMaybes $ zipWith (\i x -> maybe Nothing (\xx -> Just (i,xx)) x) [0..] xs
        base = map (\i -> startn * i - starti) [1..]

main :: IO ()
--main = interact (show . solve1 . fmap catMaybes . readInput)
main = interact (show . solve2 . snd . readInput)
