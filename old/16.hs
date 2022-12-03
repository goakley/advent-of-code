rawInput :: String
rawInput = "01111001100111011"

input :: [Bool]
input = map (=='1') rawInput

size :: Int
size = 272

size2 :: Int
size2 = 35651584

expand :: [Bool] -> [Bool]
expand [] = []
expand xs = xs ++ False : map not (reverse xs)

full :: Int -> [Bool]
full n = take n $ head $ dropWhile ((<n) . length) $ iterate expand input

shrink :: [Bool] -> [Bool]
shrink (x:y:xs) = (x == y) : shrink xs
shrink _ = []

checksum :: Int -> [Bool]
checksum = head . dropWhile (even . length) . iterate shrink . full

ss :: [Bool] -> String
ss = map (\b -> if b then '1' else '0')

solution1 :: String
solution1 = ss $ checksum size

solution2 :: String
solution2 = ss $ checksum size2

main :: IO ()
main = print solution1 >> print solution2
