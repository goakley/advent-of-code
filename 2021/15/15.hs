import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Data.Hashable (Hashable)
import Data.Tuple (swap)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set

-- convenience factor +2
type Map = Map.HashMap
type Set = Set.Set

data Cost = Value Int | Infinite deriving (Eq)

instance Ord Cost where
  compare Infinite Infinite = EQ
  compare Infinite (Value _) = GT
  compare (Value _) Infinite = LT
  compare (Value a) (Value b) = compare a b

-- |priority queue in the most rudimentary way possible
-- storing values `v` with a priority of `p`
type PQueue p v = (Set (p,v), Map v p)

pqMinView :: (Eq v, Hashable v) => PQueue p v -> Maybe (v, PQueue p v)
pqMinView (a,b) = (\((_,k),a') -> (k,(a',Map.delete k b))) <$> Set.minView a

pqInsert :: (Eq v, Hashable v, Ord p, Ord v) => (p,v) -> PQueue p v -> PQueue p v
pqInsert (v,xy) (a,b) =
  maybe
  (Set.insert (v,xy) a,Map.insert xy v b)
  (\o -> (Set.insert (v,xy) (Set.delete (o,xy) a),Map.insert xy v b))
  (Map.lookup xy b)

pqFromList :: (Eq v, Hashable v, Ord p, Ord v) => [(p,v)] -> PQueue p v
pqFromList = foldr pqInsert (Set.empty,Map.empty)

-- |Get all the points adjacent to a given point
adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- |Fill out the distances (total cost) map for the remaining locations
dijkstra' :: Map (Int,Int) Int -> Map (Int,Int) Int -> PQueue Int (Int,Int) -> Map (Int,Int) Int
dijkstra' costs distances remaining =
  maybe
  distances
  (\(xy,remaining'') -> let (d'',r'') = foldr (\a (distances',remaining') -> let ncostM = liftA2 (+) (Map.lookup xy distances) (Map.lookup a costs)
                                                                                 ocostM = Map.lookup a distances
                                                                             in case (ncostM, ocostM) of
                                                                                  (Just ncost, Just ocost) -> if ncost < ocost
                                                                                                              then (Map.insert a ncost distances',pqInsert (ncost,a) remaining')
                                                                                                              else (distances',remaining')
                                                                                  _ -> (distances',remaining')
                                              ) (distances,remaining'') (adjacent xy)
                        in dijkstra' costs d'' r''
  ) (pqMinView remaining)

-- |Generate a mapping from location to total-distance-from-point
dijkstra :: Map (Int,Int) Int -> (Int,Int) -> Map (Int,Int) Int
dijkstra m start = dijkstra' m distances remaining
  -- TODO: don't use 999999 placeholder thingy, use Maybe
  where distances = Map.mapWithKey (\k -> const (if k == start then 0 else 999999)) m
        remaining = pqFromList $ map swap $ Map.toList distances

-- |Get the cost of the cheapest path between the start and end of the cavern
cheapestCost :: Map (Int,Int) Int -> Int
cheapestCost m = (Map.! end) (dijkstra m start)
  where start = (minimum . map fst &&& minimum . map snd) $ Map.keys m
        end = (maximum . map fst &&& maximum . map snd) $ Map.keys m

-- |Expand a map based on the rules of the problem
extrapolate :: Int -> Map (Int,Int) Int -> Map (Int,Int) Int
extrapolate n m = Map.foldrWithKey (\(x,y) z a -> foldr
                                                  (\(v,w) -> Map.insert (x+i*v,y+j*w) (((z+v+w-1) `mod` 9) + 1))
                                                  a
                                                  [(v,w) | v <- [0..(n-1)], w <- [0..(n-1)]]
                                   ) Map.empty m
  where (i,j) = (maximum . map fst &&& maximum . map snd) $ Map.keys m

solve1 :: Map (Int,Int) Int -> Int
solve1 = cheapestCost

solve2 :: Map (Int,Int) Int -> Int
solve2 = cheapestCost . extrapolate 5

readInput :: String -> Map (Int,Int) Int
readInput = Map.fromList
          . concat
          . zipWith (\x -> zipWith (\y c -> ((x,y),read [c])) [1..]) [1..]
          . lines

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
