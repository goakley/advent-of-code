{-# LANGUAGE TupleSections #-}
import Data.Maybe (mapMaybe)
import Text.Parsec (char, digit, many1, optionMaybe, runParser, string)
import Text.Parsec.String (Parser)
import Control.Arrow ((&&&))

-- |A Data.Ix range, but with the potential to go infinite
type IRange = (Int, Maybe Int)

-- |Get the amount of overlap between an irange and a range
ioverlap :: IRange -> (Int,Int) -> Int
ioverlap (b,Nothing) (a,c) = ioverlap (b,Just c) (a,c)
ioverlap (b,Just d) (a,c) = max 0 ((min c d - max a b) + 1)

-- |Convert a list the range which the list covers (min,max)
l2range :: [Int] -> Maybe (Int, Int)
l2range [] = Nothing
l2range xs = Just (minimum xs, maximum xs)

-- |Convert a list to the irange which the list covers (min,max)
l2irange :: Int -> [Int] -> Maybe (Int, Maybe Int)
l2irange _ [] = Nothing
l2irange x xs = Just (mini, if x == maxi then Nothing else Just maxi)
  where mini = minimum xs
        maxi = maximum xs

-- |Get the sign (-1,0,1) of a number
sign :: Int -> Int
sign n = if n < 0 then -1 else 1

-- |Get the triangle value (A000217) for a number, allowing for negatives
-- ( if f(n) = x , f(-n) = -x )
triangle :: Int -> Int
triangle n = sign n * n * (n + sign n) `div` 2

-- |Get the furthest travel distance for a given starting x velocity
xpos :: Int -> Int
xpos = triangle

-- |Get the maximum height for a given starting y velocity
peak :: Int -> Int
peak = max 0 . triangle

-- |Get the x positions which end up inside the area
-- along with the range of steps during which they're inside the area
getxes :: (Int,Int) -> [(Int,(Int,Maybe Int))]
getxes (xmin,xmax) = mapMaybe (\x -> fmap (x,)
                                   $ l2irange x
                                   $ map (\v -> abs x - v)
                                   $ filter (\u -> let v = xpos x - sign x * xpos u in v >= xmin && v <= xmax)
                                   [0..abs x]
                         ) [-big..big]
  where big = max (abs xmin) (abs xmax)

-- |Get the y positions which end up inside the area
-- along with the range of steps during which they're inside the area
getyes :: (Int,Int) -> [(Int,(Int,Int))]
getyes (ymin,ymax) = if ymax > 0
                     then error "I dunno how to do this with a positive trench"
                     else mapMaybe (\y -> fmap (y,)
                                        $ l2range
                                        $ map (\(i,_,_) -> i)
                                        $ filter (\(_,v,_) -> v >= ymin && v <= ymax)
                                        $ takeWhile (\(_,y',g) -> g > 0 || y' >= ymin)
                                        $ iterate (\(i,y',g) -> (i+1,y'+g,g-1))
                                        (0,0,y)
                                   ) [-big..big]
  where big = max (abs ymin) (abs ymax)

solve1 :: ((Int,Int),(Int,Int)) -> Int
solve1 (_,(ymin,ymax)) = peak $ maximum $ map fst $ filter (\(_,xs) -> not (null xs)) $ getyes (ymin,ymax)

solve2 :: ((Int,Int),(Int,Int)) -> Int
solve2 ((xmin,xmax),(ymin,ymax)) = sum $ map (\xbounds -> sum $ map (min 1 . ioverlap xbounds) ys) xs
  where xs = map snd $ getxes (xmin,xmax)
        ys = map snd $ getyes (ymin,ymax)

parseInt :: Parser Int
parseInt = do
  neg <- optionMaybe (char '-')
  d <- many1 digit
  return $ read $ maybe d (const ('-':d)) neg

parseInput :: Parser ((Int,Int),(Int,Int))
parseInput = do
  _ <- string "target area: x="
  xmin <- parseInt
  _ <- string ".."
  xmax <- parseInt
  _ <- string ", y="
  ymin <- parseInt
  _ <- string ".."
  ymax <- parseInt
  return ((xmin,xmax),(ymin,ymax))

readInput :: String -> ((Int,Int),(Int,Int))
readInput = either (error "unsovlable") id . runParser parseInput () ""

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
