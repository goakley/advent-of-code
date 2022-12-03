import qualified Data.Map as Map

border :: (Int,Int) -> [(Int,Int)]
border (x,y) = [(xx,yy) | xx <- [x-1..x+1], yy <- [y-1..y+1], xx /= x || yy /= y]

readInput :: String -> Map.Map (Int,Int) Bool
readInput xs = foldl (\a (x,l) -> foldr (\(y,c) -> if c == 'L' then Map.insert (x,y) False else id) a (zip [0..] l)) Map.empty $ zip [0..] $ lines xs

runonce :: Map.Map (Int,Int) Bool -> Map.Map (Int,Int) Bool
runonce m = Map.mapWithKey (\(x,y) v -> let cnt = length (filter (flip (Map.findWithDefault False) m) (border (x,y)))
                                        in if v then cnt < 4 else cnt < 1
                           ) m

run :: Map.Map (Int,Int) Bool -> Map.Map (Int,Int) Bool
run m = let m' = runonce m in if m == m' then m else run m'

solve1 :: Map.Map (Int,Int) Bool -> Int
solve1 = length . filter id . Map.elems . run

main :: IO ()
main = interact (show . solve1 . readInput)
