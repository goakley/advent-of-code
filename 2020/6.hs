import qualified Data.Set as Set

readInput :: String -> [[String]]
readInput input = (\(a,b) -> a:b) $ foldl (\(aa,ab) b -> if null b then ([],(aa:ab)) else (b:aa,ab)) ([],[]) $ lines input

solve1 :: [[String]] -> Int
solve1 = sum . map (Set.size . foldl (\a -> Set.union a . Set.fromList) Set.empty)

solve2 :: [[String]] -> Int
solve2 = sum . map (Set.size . foldl (\a -> Set.intersection a . Set.fromList) (Set.fromList ['a'..'z']))

main :: IO ()
--main = interact (show . solve1 . readInput)
main = interact (show . solve2 . readInput)
