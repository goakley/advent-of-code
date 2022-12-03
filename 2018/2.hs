import Data.List (sort, sortOn)
import qualified Data.Map
import qualified Data.Maybe

readLine :: String -> Int
readLine ('+':xs) = read xs
readLine ('-':xs) = (-1) * read xs
readLine _ = undefined

readInput :: String -> [Int]
readInput = map readLine . lines

count :: String -> Data.Map.Map Char Int
count = foldr (Data.Map.alter (Just . maybe 1 (+1))) Data.Map.empty

analyse :: String -> (Bool,Bool)
analyse xs = (twos,threes)
  where counts = count xs
        twos = Data.Map.foldl (\b x -> b || x == 2) False counts
        threes = Data.Map.foldl (\b x -> b || x == 3) False counts

checksum :: [String] -> Int
checksum xs = twos * threes
  where analysis = map analyse xs
        twos = length $ filter fst analysis
        threes = length $ filter snd analysis

locate' :: [String] -> String
locate' (a:b:xs) = let score = length (filter id (zipWith (/=) a b))
                   in if score == 1
                      then Data.Maybe.catMaybes (zipWith (\x y -> if x == y then Just x else Nothing) a b)
                      else locate' (b:xs)
locate' _ = undefined

locate :: [String] -> String
locate xs = locate' $ sortOn sort xs

main :: IO ()
--main = interact (show . checksum . lines)
main = interact (locate . lines)
