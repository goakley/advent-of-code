import Crypto.Hash.MD5 (hash)

import Data.ByteString (unpack)
import Data.Char (intToDigit)
import Data.Map (Map, empty, insertWith, size, toAscList)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)

input :: String
input = "wtnhxymk"

hashes :: [[Word8]]
hashes = filter (\(a:b:c:_) -> a == 0 && b == 0 && c < 16) $
         map (unpack . hash . encodeUtf8 . pack . (input ++) . show) [(0::Int)..]

solution1 :: String
solution1 = take 8 $
            map (\(_:_:a:_) -> intToDigit (fromIntegral a)) hashes

accum :: Map Word8 Char -> [(Word8,Char)] -> String
accum m ((a,b):xs) = if size m == 8 then map snd (toAscList m) else accum m' xs
  where m' = insertWith (curry snd) a b m
accum _ _ = undefined

solution2 :: String
solution2 = accum empty $
            filter (\(a,_) -> a >= 0 && a <= 7) $
            map (\(_:_:a:b:_) -> (a, intToDigit (fromIntegral b `div` 16))) hashes

main :: IO ()
main = print solution1 >> print solution2
