import qualified Data.Set as Set

data Grid = Grid (Set.Set (Int,Int)) (Int,Int) deriving Show

isTree :: Grid -> (Int,Int) -> Bool
isTree (Grid s (mx,_)) (x,y) = (a,y) `Set.member` s
  where a = x `mod` mx

readLine :: String -> [Int]
readLine [] = []
readLine xs = map snd $ filter ((=='#') . fst) $ zip xs [0..]

build :: [(Int,[Int])] -> Set.Set (Int,Int)
build [] = Set.empty
build ((y,xs):ys) = foldl (\a x -> Set.insert (x,y) a) (build ys) xs

readInput :: String -> Set.Set (Int,Int)
readInput = build . zip [0..] . map readLine . lines

createGrid :: Set.Set (Int,Int) -> Grid
createGrid s = Grid s (mx,my)
  where mx = 31
        my = 323

runGrid :: Grid -> (Int,Int) -> Int
runGrid (Grid s (mx,my)) (x,y) = length $ filter (isTree (Grid s (mx,my))) steps
  where steps = [(a*x,a*y) | a <- [0..my]]

solve1 :: Grid -> Int
solve1 = flip runGrid (3,1)

solve2 :: Grid -> Int
solve2 g = product $ map (runGrid g) [(1,1),(3,1),(5,1),(7,1),(1,2)]

main :: IO ()
--main = interact (show . solve1 . createGrid . readInput)
main = interact (show . solve2 . createGrid . readInput)
