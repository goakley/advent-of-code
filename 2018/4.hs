import Data.Ix
import Data.List
import qualified Data.Map
import Data.Ord
import Text.Regex.TDFA

type Date = (Int,Int,Int,Int,Int)

data Action = Shift Int | Sleep | Wake deriving (Eq,Ord,Show)

getAction :: String -> Action
getAction "wakes up" = Wake
getAction "falls asleep" = Sleep
getAction s = case s =~ "^Guard #([0-9]+) begins shift$" of
                [[_,x]] -> Shift (read x)
                _ -> undefined

readLine :: String -> (Date,Action)
readLine input = ((read y,read m,read d,read th,read tm),getAction a)
  where pattern = "^\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] (.+)$"
        [[_,y,m,d,th,tm,a]] = input =~ pattern

coallate :: Int -> [(Date,Action)] -> Data.Map.Map Int [(Date,Date)]
coallate _ [] = Data.Map.empty
coallate n ((d1,Sleep):(d2,Wake):xs) = Data.Map.alter (Just . maybe [(d1,d2)] ((d1,d2):)) n (coallate n xs)
coallate _ ((_,Shift i):xs) = coallate i xs
coallate _ _ = undefined

--solve1 :: [(Date,Action)] -> Int
solve1 xs = (\((_,_,_,_,a),(_,_,_,_,b)) -> range (a,b-1))
  where cs = coallate 0 (sort xs)
        can = maximumBy (comparing (sum . map (\((_,_,_,_,a),(_,_,_,_,b)) -> b - a) . snd)) (Data.Map.toList cs)

main :: IO ()
main = interact (show . solve1 . map readLine . lines)
