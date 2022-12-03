{-# LANGUAGE TupleSections #-}
import Data.Functor (($>))
import Data.Ix (range)
import Text.Parsec (char, digit, eof, many, many1, newline, optionMaybe, runParser, string, try, (<|>))
import Text.Parsec.String (Parser)
import qualified Data.HashSet as Set

-- convenience factor +1
type Set = Set.HashSet

type Point = (Int,Int,Int)

type PointCube = Set Point

boundPoint :: (Point,Point) -> Point -> Point
boundPoint ((xmin,ymin,zmin),(xmax,ymax,zmax)) (x,y,z) = (max xmin (min xmax x), max ymin (min ymax y), max zmin (min zmax z))

boundPoint1 :: Point -> Point
boundPoint1 = boundPoint ((-50,-50,-50),(50,50,50))

type Area = (Point,Point)

-- |"Touching" counts as overlap because i'm a bad person
overlap :: (Int,Int) -> (Int,Int) -> Bool
overlap (b,d) (a,c) = let amount = max 0 ((min c d - max a b) + 2) in amount > 0

whut xmin1 xmax1 xmin2 xmax2 = if xmin1 > xmin2
                               then whut xmin2 xmax2 xmin1 xmax1
                               else if overlap (xmin1,xmax2) (xmin2,xmax)
                                    then _
                                    else [(xmin1,xmax1),(xmax1]

combineAreas :: Area -> Area -> [Area]
combineAreas a1@(ps1,pe1) a2@(ps2,pe2) = case (inRange a1 ps2, inRange a1 pe2, inRange a2 ps1, inRange a2 pe1) of
                                           (True,True,_,_) -> [a1]
                                           (_,_,True,True) -> [a2]
                                           (True,False,?,?) -> _
                                           (False,True,?,?) -> _
                                           (False,False,False,False) -> [a1,a2]

merge :: [Area] -> Area -> [Area]
merge = foldl (\a (ps,pe) -> case (inRange) [] area



applyRule1 :: (Bool,(Point,Point)) -> PointCube -> PointCube
applyRule1 (o,(a,b)) s = let a' = boundPoint1 a
                             b' = boundPoint1 b
                         in foldr (if o then Set.insert else Set.delete) s $ range (a',b')

solve1 :: [(Bool,(Point,Point))] -> Int
solve1 = Set.size . foldl (flip applyRule1) Set.empty

parseInt :: Parser Int
parseInt = do
  neg <- optionMaybe (char '-')
  d <- many1 digit
  return $ read $ maybe d (const ('-':d)) neg

parseRange :: Parser (Int,Int)
parseRange = (,) <$> (parseInt <* string "..") <*> parseInt

parseLine :: Parser (Bool,(Point,Point))
parseLine = do
  b <- try (string "on" $> True) <|> (string "off" $> False)
  _ <- string " x="
  (xmin,xmax) <- parseRange
  _ <- string ",y="
  (ymin,ymax) <- parseRange
  _ <- string ",z="
  (zmin,zmax) <- parseRange
  _ <- newline
  return (b,((xmin,ymin,zmin),(xmax,ymax,zmax)))

parseInput :: Parser [(Bool,(Point,Point))]
parseInput = many parseLine <* eof

readInput :: String -> [(Bool,(Point,Point))]
readInput = either (error . show) id . runParser parseInput () ""

main = interact (show . solve1 . readInput)
