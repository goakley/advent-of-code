import Data.Char (intToDigit)
import Crypto.Hash.MD5 (hash)

import Data.ByteString (foldr)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

input :: String
input = "vwbaicqe"

md5sum :: String -> String
md5sum = Data.ByteString.foldr (\w a -> intToDigit (fromIntegral (w `div` 16)) : intToDigit (fromIntegral (w `mod` 16)) : a) [] . hash . encodeUtf8 . pack

opened :: Char -> Bool
opened = (`elem` "bcdef")

subs :: ((Int,Int),String) -> [((Int,Int),String)]
subs ((x,y),s) = concat [up,down,left,right]
  where (u:d:l:r:_) = map opened $ md5sum s
        up = [((x,y-1),s++"U") | y > 0 && u]
        down = [((x,y+1),s++"D") | y < 3 && d]
        left = [((x-1,y),s++"L") | x > 0 && l]
        right = [((x+1,y),s++"R") | x < 3 && r]

shortest :: [((Int,Int),String)] -> String
shortest (((x,y),s):poss) = if (x,y) == (3,3) then s else shortest (poss ++ subs ((x,y),s))
shortest _ = undefined

solution1 :: String
solution1 = drop (length input) $ shortest [((0,0),input)]

longest :: [((Int,Int),String)] -> String -> String
longest [] m = m
longest (((x,y),s):poss) m = if (x,y) == (3,3) then longest poss m' else longest (poss ++ subs ((x,y),s)) m
  where m' = if length m < length s then s else m

solution2 :: Int
solution2 = length $ drop (length input) $ longest [((0,0),input)] ""

main :: IO ()
main = print solution1 >> print solution2
