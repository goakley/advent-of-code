import Text.Regex.TDFA

import Data.Char (isDigit)
import qualified Data.Map as Map
import qualified Data.Set as Set

readLine :: String -> [(String,String)]
readLine input = xs
  where pat = "([a-zA-Z0-9]+):([a-zA-Z0-9#]+)"
        xs = map (\[_,k,v] -> (k,v)) $ input =~ pat

readInput :: String -> [[(String,String)]]
readInput input = uncurry (:) $ foldl (\(aa,ab) b -> if null b then ([],aa:ab) else (aa++b,ab)) ([],[]) $ map readLine $ lines input

solve1 :: [[(String,String)]] -> Int
solve1 xss = length $ filter (\xs -> let s = Set.fromList (map fst xs) in all (`Set.member` s) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]) xss

pHgt :: String -> Bool
pHgt s = not (null match) && (if (head match !! 2) == "cm" then okcm else okin) (read (head match !! 1) :: Int)
  where match = (s =~ "([0-9]+)(cm|in)") :: [[String]]
        okcm a = a >= 150 && a <= 193
        okin a = a >= 59 && a <= 76

fns :: Map.Map String (String -> Bool)
fns = Map.fromList [("byr", \x -> length x == 4 && all isDigit x && read x >= (1920::Int) && read x <= (2002::Int))
                   ,("iyr", \x -> length x == 4 && all isDigit x && read x >= (2010::Int) && read x <= (2020::Int))
                   ,("eyr", \x -> length x == 4 && all isDigit x && read x >= (2020::Int) && read x <= (2030::Int))
                   ,("hgt", pHgt)
                   ,("hcl", (=~ "^#[0-9a-f]{6}$"))
                   ,("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
                   ,("pid", \x -> length x == 9 && all isDigit x)
                   ]

solve2 :: [[(String,String)]] -> Int
solve2 xss = length $ filter (\xs -> let s = Set.fromList (map fst xs) in all (`Set.member` s) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] && all (\(k,v) -> maybe True (\f -> f v) $ Map.lookup k fns) xs) xss

main :: IO ()
--main = interact (show . solve1 . readInput)
main = interact (show . solve2 . readInput)
