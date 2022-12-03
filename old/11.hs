import Debug.Trace (trace)

import Data.Array (Array, bounds, elems, listArray, (!), (//))
import Data.Either (rights)
import Data.Map (Map, empty, insert, lookup, singleton, (!))
import Data.Set (Set, delete, elems, empty, fromList, insert, member, toList, unions)

import Text.Regex.TDFA ((=~))

rawInput :: String
rawInput = unlines [
  "The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.",
  "The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.",
  "The third floor contains nothing relevant.",
  "The fourth floor contains nothing relevant."]
{-
rawInput = unlines [
  "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.",
  "The second floor contains a hydrogen generator.",
  "The third floor contains a lithium generator.",
  "The fourth floor contains nothing relevant."]
-}

rawInput2 :: String
rawInput2 = unlines [
  "The first floor contains an elerium generator, an elerium-compatible microchip, a dilithium generator, a dilithium-compatible microchip, a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.",
  "The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.",
  "The third floor contains nothing relevant.",
  "The fourth floor contains nothing relevant."]

readLine :: String -> Set (Either String String)
readLine s = fromList $ map (\[_,e,t] -> (if head t == '-' then Left else Right) e) r
  where r = s =~ "an? ([a-z]+)( generator|-compatible microchip)"

type State = (Int, Array Int (Set (Either String String)))

input :: State
input = let es = map readLine (lines rawInput) in (1, listArray (1,length es) es)

input2 :: State
input2 = let es = map readLine (lines rawInput2) in (1, listArray (1,length es) es)

target :: State
target = (h, listArray (l,h) es)
  where (l,h) = bounds (snd input)
        es = reverse $ unions (Data.Array.elems (snd input)) : replicate (h-l) Data.Set.empty

isValidFloor :: Set (Either String String) -> Bool
isValidFloor s = safe || covered
  where gs = rights (toList s)
        safe = all (\g -> (Left g) `member` s) gs
        covered = all (either (\m -> (Right m) `member` s) (const True)) (toList s)

isValid :: State -> Bool
isValid (_,fs) = all isValidFloor fs

subStates :: State -> [State]
subStates (i,fs) = filter isValid $ map (\(x,y) -> (i+1,fs//[(i,x),(i+1,y)])) ups ++ map (\(x,y) -> (i-1,fs//[(i-1,y),(i,x)])) downs
  where f = fs Data.Array.! i
        u = fs Data.Array.! (i + 1)
        d = fs Data.Array.! (i - 1)
        targets = sequence [Data.Set.elems f, Data.Set.elems f]
        (l,h) = bounds fs
        ups = if i < h then map (\ts -> (foldr Data.Set.delete f ts,foldr Data.Set.insert u ts)) targets else []
        downs = if i > l then map (\ts -> (foldr Data.Set.delete f ts,foldr Data.Set.insert d ts)) targets else []

cost :: [(Int,State)] -> Set State -> Int
cost ((c,s):ss) seen
  | s == target = c
  | s `member` seen = cost ss seen
  | otherwise = cost (ss ++ map (\s' -> (c+1,s')) (subStates s)) (Data.Set.insert s seen)
cost [] _ = undefined

solution1 :: Int
solution1 = cost [(0,input)] Data.Set.empty

solution2 :: Int
solution2 = cost [(0,input2)] Data.Set.empty

main :: IO ()
main = print solution2
