import Control.Arrow ((&&&))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map

-- convenience factor +1
type Map = Map.HashMap

-- |Hell with storing the entire sequence - just treat a polymer as a count of its pairs
type Polymer = Map (Char,Char) Int

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

-- |Add a counter value to a map - the value sums with the existing value if one exists
addCountToMap :: (Eq a, Hashable a) => Int -> a -> Map a Int -> Map a Int
addCountToMap v = Map.alter (Just . maybe v (+v))

-- |Count the number of elements in a list
count :: (Eq a, Hashable a) => [a] -> Map a Int
count = foldr (addCountToMap 1) Map.empty

-- |Turn a count of pairs into a count of each element
unpairCounts :: (Eq a, Hashable a) => (a,a) -> Map (a,a) Int -> Map a Int
unpairCounts (left,right) = Map.map (`div` 2)
                          . Map.foldrWithKey
                            (\(k1,k2) v -> addCountToMap v k1
                                         . addCountToMap v k2)
                            (Map.fromList [(left,1),(right,1)])

-- |Use a lookup table of inserts to polymerise a polymer
polymerise :: Map (Char,Char) Char -> Polymer -> Polymer
polymerise i = Map.foldrWithKey
               (\(a,b) c s' -> maybe
                               (Map.insert (a,b) c s')
                               (\r -> addCountToMap c (a,r)
                                    $ addCountToMap c (r,b) s')
                               (Map.lookup (a,b) i))
               Map.empty

-- |Polymerise a polymer some number of times then do the crazy math the problem asks for
countAndSubtractPolymerisations :: Int -> ((Char,Char), Polymer, Map (Char,Char) Char) -> Int
countAndSubtractPolymerisations polymerisations (ends,polymer,insertions)
  = uncurry (-)
  $ (maximum &&& minimum)
  $ Map.elems
  $ unpairCounts ends
  $ last
  $ take (polymerisations + 1)
  $ iterate (polymerise insertions) polymer

solve1 :: ((Char,Char), Polymer, Map (Char,Char) Char) -> Int
solve1 = countAndSubtractPolymerisations 10

solve2 :: ((Char,Char), Polymer, Map (Char,Char) Char) -> Int
solve2 = countAndSubtractPolymerisations 40

readInput :: String -> ((Char,Char), Polymer, Map (Char,Char) Char)
readInput input = (ends, pairs, insertions)
  where [[template],list] = splitOn "" $ lines input
        ends = (head &&& last) template
        pairs = count $ zip template $ tail template
        insertions = Map.fromList $ map (\x -> let [[a,b],_,[c]] = words x in ((a,b),c)) list

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
