import Data.Ix
import qualified Data.Map
import Text.Regex.TDFA

readLine :: String -> (String,((Int,Int),(Int,Int)))
readLine input = (a,((read l,read t),(read w,read h)))
  where pattern = "^#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)$"
        [[_,a,l,t,w,h]] = input =~ pattern

rangify :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
rangify ((l,t),(w,h)) = range ((l,t),(l+(w-1),t+(h-1)))

count :: [(String,((Int,Int),(Int,Int)))] -> Data.Map.Map (Int,Int) Int
count [] = Data.Map.empty
count ((_,((l,t),(w,h))):xs) = foldr (Data.Map.alter (Just . maybe 1 (+1))) (count xs) (rangify ((l,t),(w,h)))

surmise :: [(String,((Int,Int),(Int,Int)))] -> Int
surmise = length . filter (>1) . Data.Map.elems . count

find :: [(String,((Int,Int),(Int,Int)))] -> String
find xs = fst $ head $ filter okay xs
  where cnt = count xs
        okay (_,ltwh) = all (\i -> Data.Map.lookup i cnt == Just 1) (rangify ltwh)

main :: IO ()
--main = interact (show . surmise . map readLine . lines)
main = interact (find . map readLine . lines)
