import Data.Char (intToDigit)
import Data.Maybe (listToMaybe)
import Crypto.Hash.MD5 (hash)

import Control.Arrow ((&&&))

import Data.ByteString (foldr)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

input :: String
input = "qzyelonm"

-- |Calculate the md5sum of a UTF-8 string as hex digits
md5sum :: String -> String
md5sum = Data.ByteString.foldr (\w a -> intToDigit (fromIntegral (w `div` 16)) : intToDigit (fromIntegral (w `mod` 16)) : a) [] . hash . encodeUtf8 . pack

-- |All possible n-md5sum hashes
hashes :: Int -> [String]
hashes n = map ((!!n) . iterate md5sum . (input ++) . show) [(0::Int)..]

-- |Extract all values that appear at least n times in a row
series :: Eq a => Int -> [a] -> [a]
series _ [] = []
series n (x:xs)
  | length ys < (n - 1) = []
  | all (==x) ys = x : series n xs
  | otherwise = series n xs
  where ys = take (n - 1) xs

-- |Extract the first value that appears at least 3 times in a row
triple :: Eq a => [a] -> Maybe a
triple = listToMaybe . series 3

-- |Extract all values that appear at least 5 times in a row
quintuples :: Eq a => [a] -> [a]
quintuples = series 5

massage :: Eq a => [a] -> (Maybe a,[a])
massage = triple &&& quintuples

pads :: Eq a => Int -> [(Maybe a,[a])] -> [Int]
pads _ [] = []
pads n ((Nothing,_):xs) = pads (n + 1) xs
pads n ((Just x,_):xs) = (if x `elem` concatMap snd (take 1000 xs) then (n:) else id) $ pads (n + 1) xs

solve :: Int -> Int
solve = (!!63) . pads 0 . map massage . hashes

solution1 :: Int
solution1 = solve 1

solution2 :: Int
solution2 = solve 2017

main :: IO ()
main = print solution1 >> print solution2
