import Control.Arrow ((&&&))
import qualified Data.Map as Map

-- |Get the sign of the number (-1, 0, 1)
sign :: Int -> Int
sign x = case compare x 0 of
  EQ -> 0
  GT -> 1
  LT -> -1

-- |Count the number of elements in a list
count :: Ord a => [a] -> Map.Map a Int
count = foldr (Map.alter (Just . maybe (1::Int) (+1))) Map.empty

type Point = (Int,Int)

-- |Create all the points along a line specified by two points
--
-- Only horizontal, vertical, and optionally diagonal lines are supported
-- (other types of lines will be ignored).
pointsToLine :: Bool -> (Point,Point) -> [Point]
pointsToLine useDiagonal ((a,b),(c,d)) = case (compare a c, compare b d) of
  (EQ, _) -> [(a,y) | y <- [(min b d)..(max b d)]]
  (_, EQ) -> [(x,b) | x <- [(min a c)..(max a c)]]
  (_, _)  -> if useDiagonal && (abs (a - c) == abs (b - d))
             then [(a + (i * sign (c - a)), b + (i * sign (d - b))) | i <- [0..(abs (a - c))]]
             else [] -- ignore all other types of lines

-- |Count the number of points that lie on more than one line
overlappingPointCount :: Bool -> [(Point,Point)] -> Int
overlappingPointCount useDiagonal = length
                                  . filter (>1)
                                  . Map.elems
                                  . count
                                  . concatMap (pointsToLine useDiagonal)

solve1 :: [(Point,Point)] -> Int
solve1 = overlappingPointCount False

solve2 :: [(Point,Point)] -> Int
solve2 = overlappingPointCount True

readLine :: String -> (Point,Point)
readLine input = ((read a, read b), (read c, read d))
  where [l,_,r] = words input
        (a,b) = drop 1 <$> break (==',') l
        (c,d) = drop 1 <$> break (==',') r

main :: IO ()
main = interact (show . (solve1 &&& solve2) . map readLine . lines)
