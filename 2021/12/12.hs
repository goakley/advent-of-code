import Control.Arrow ((&&&))
import Data.Char (isLower)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

-- convenience factor +2
type Map = Map.HashMap
type Set = Set.HashSet

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

-- |Navigate the caves, determining how many possible paths there are to the end
--
-- This function will not swim through caves that appear in `visited` unless `revisit` is set.
-- If `revisit` is set, it will visit at most one member of `visited` one time.
walk :: String -> Set String -> Bool -> Map String [String] -> Int
walk cave visited revisit connections
  | cave == "end"           = 1
  | Set.member cave visited = if revisit then walkChildren False else 0
  | otherwise               = walkChildren revisit
  where children = filter (/="start") $ Map.lookupDefault [] cave connections
        visited' = (if all isLower cave then Set.insert cave else id) visited
        walkChildren revisit' = sum $ map (\cave' -> walk cave' visited' revisit' connections) children

-- |Navigate virgin caves from the start
walkFromStart :: Bool -> Map String [String] -> Int
walkFromStart = walk "start" Set.empty

solve1 :: Map String [String] -> Int
solve1 = walkFromStart False

solve2 :: Map String [String] -> Int
solve2 = walkFromStart True

readLine :: String -> (String,String)
readLine xs = let [l,r] = splitOn '-' xs in (l,r)

readInput :: String -> Map String [String]
readInput = foldr ((\(l,r) -> Map.alter (Just . maybe [l] (l:)) r
                            . Map.alter (Just . maybe [r] (r:)) l
                   ) . readLine
                  ) Map.empty
          . lines

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
