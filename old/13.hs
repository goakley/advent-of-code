import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map
import qualified Data.Set

import Data.Char (intToDigit)

import Numeric (showIntAtBase)

input :: Int
input = 1364

target :: (Int,Int)
target = (31,39)

isOpen :: (Int,Int) -> Bool
isOpen (x,y) = x >= 0 && y >= 0 && even (length $ filter (=='1') b)
  where m = x * x + 3 * x + 2 * x * y + y + y * y
        o = m + input
        b = showIntAtBase 2 intToDigit o ""

type Heap = (Set (Int,(Int,Int)), Map (Int,Int) Int)

heapSingleton :: (Int,Int) -> Int -> Heap
heapSingleton xy c = (Data.Set.singleton (c,xy),Data.Map.singleton xy c)

heapMin :: Heap -> Maybe ((Int,(Int,Int)), Heap)
heapMin (h,m) = (\((c,xy),h') -> ((c,xy),(h',Data.Map.delete xy m))) <$> Data.Set.minView h

heapPrioritize :: (Int,Int) -> Int -> Heap -> Heap
heapPrioritize xy c (h,m) = (h',m')
  where h' = Data.Set.insert (c,xy) (maybe h (\c' -> Data.Set.delete (c',xy) h) (Data.Map.lookup xy m))
        m' = Data.Map.insert xy c m

traceback :: (Int,Int) -> Map (Int,Int) (Int,Int) -> [(Int,Int)]
traceback xy m = maybe [xy] (\xy' -> xy : traceback xy' m) $ Data.Map.lookup xy m

dijkstra :: Set (Int,Int) -> (Heap, Map (Int,Int) Int, Map (Int,Int) (Int,Int)) -> [(Int,Int)]
dijkstra seen (heap,dist,prev) =
  case heapMin heap of
   Nothing -> undefined
   Just ((c,(x,y)),heap') | (x,y) == target -> reverse $ traceback target prev
                          | ((x,y) `Data.Set.member` seen) || not (isOpen (x,y)) -> dijkstra seen (heap',dist,prev)
                          | otherwise -> dijkstra (Data.Set.insert (x,y) seen) $ foldl (\(h,d,p) xy -> if maybe True ((c+1)<) (Data.Map.lookup xy dist) then (heapPrioritize xy (c+1) h,Data.Map.insert xy (c+1) d,Data.Map.insert xy (x,y) p) else (h,d,p)) (heap',dist,prev) $ filter isOpen [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

solution1 :: Int
solution1 = subtract 1 $ length $ dijkstra Data.Set.empty (heapSingleton (1,1) 0,Data.Map.singleton (1,1) 0,Data.Map.empty)

reach :: Int
reach = 50

dijkstra2 :: Set (Int,Int) -> (Heap, Map (Int,Int) Int) -> Set ((Int,Int),Int)
dijkstra2 seen (heap,dist) =
  case heapMin heap of
   Nothing -> Data.Set.fromList $ filter ((<=reach) . snd) $ Data.Map.toList dist
   Just ((c,(x,y)),heap') | c > reach || ((x,y) `Data.Set.member` seen) || not (isOpen (x,y)) -> dijkstra2 seen (heap',dist)
                          | otherwise -> dijkstra2 (Data.Set.insert (x,y) seen) $ foldl (\(h,d) xy -> if maybe True ((c+1)<) (Data.Map.lookup xy dist) then (heapPrioritize xy (c+1) h,Data.Map.insert xy (c+1) d) else (h,d)) (heap',dist) $ filter isOpen [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

solution2 :: Int
solution2 = length $ dijkstra2 Data.Set.empty (heapSingleton (1,1) 0,Data.Map.singleton (1,1) 0)

main :: IO ()
main = print solution1 >> print solution2
