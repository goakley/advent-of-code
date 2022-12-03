import Control.Arrow ((&&&), (***))
import qualified Data.HashSet as Set

-- convenience factor +1
type Set = Set.HashSet

data Crease = X | Y deriving (Eq, Show)

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

-- |Perform a crease of the paper at the given position
crease :: Set (Int,Int) -> (Crease,Int) -> Set (Int,Int)
crease s (X,i) = Set.map (\(x,y) -> (if x < i then x else i - (x - i),y)) s
crease s (Y,i) = Set.map (\(x,y) -> (x,if y < i then y else i - (y - i))) s

solve1 :: (Set (Int,Int), [(Crease,Int)]) -> Int
solve1 (_,[])  = error "unsolvable"
solve1 (s,c:_) = Set.size $ crease s c

solve2 :: (Set (Int,Int), [(Crease,Int)]) -> String
solve2 (s,cs) = let result = foldl crease s cs
                    (xmin,ymin) = foldl1 (\(x,y) -> min x *** min y) result
                    (xmax,ymax) = foldl1 (\(x,y) -> max x *** max y) result
                in concatMap (\y -> map (\x -> if Set.member (x,y) result then '#' else ' '
                                        ) [xmin..xmax] ++ "\n"
                             ) [ymin..ymax]

readInput :: String -> (Set (Int,Int), [(Crease,Int)])
readInput input = (pairs, commands)
  where [one,two] = splitOn "" $ lines input
        pairs = Set.fromList $ map (\x -> let [l,r] = splitOn ',' x in (read l, read r)) one
        commands = map (\x -> let [_,_,e] = words x
                                  [l,r] = splitOn '=' e
                              in (if l == "x" then X else Y,read r)
                       ) two

main :: IO ()
main = interact (uncurry (++) . (((++"\n") . show . solve1) &&& solve2) . readInput)
