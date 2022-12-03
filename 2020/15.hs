import Data.List (iterate')
import qualified Data.Map.Strict as Map

input :: [Int]
--input = [1,2,16,19,18,0]
input = [0,3,6]

game :: [Int]
game = init input ++ map (\(_,x,_) -> x) (iterate' (\(t,l,m) -> maybe (t+1,0,Map.insert l (t - 1) m) (\p -> (t+1,(t-1)-p,Map.insert l (t - 1) m)) (Map.lookup l m)) (length input + 1, last input, Map.fromList (init (zip input [1..]))))

solve1 :: Int
solve1 = game !! (2020 - 1)

solve2 :: Int
solve2 = game !! (30000000 - 1)

main :: IO ()
--main = print solve1
main = print solve2
--main = mapM_ print $ filter ((==1) . snd) $ zip [0..] $ take 500 game
