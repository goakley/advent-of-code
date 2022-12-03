{-# LANGUAGE TupleSections #-}
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.List (delete, sort)
import Debug.Trace (traceShow)
import Safe (maximumMay)
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Parsec (char, digit, eof, many1, manyTill, newline, optionMaybe, runParser, string, (<|>))
import Text.Parsec.String (Parser)
import Control.Arrow ((&&&))
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

-- convenience factor +1
type Map = Map.HashMap
type Set = Set.HashSet

-- i am so sorry
instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5,
          Hashable a6, Hashable a7, Hashable a8, Hashable a9) =>
         Hashable (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    hash (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        hash a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7
        `hashWithSalt` a8 `hashWithSalt` a9
    hashWithSalt s (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
        s `hashWithSalt` a1 `hashWithSalt` a2 `hashWithSalt` a3
        `hashWithSalt` a4 `hashWithSalt` a5 `hashWithSalt` a6 `hashWithSalt` a7
        `hashWithSalt` a8 `hashWithSalt` a9

removeEach :: Eq a => [a] -> [(a,[a])]
removeEach xs = zip xs (map (`delete` xs) xs)

type Point = (Int,Int,Int)

pAdd :: Point -> Point -> Point
pAdd (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

pSub :: Point -> Point -> Point
pSub (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

type Matrix = (Int,Int,Int,Int,Int,Int,Int,Int,Int)

multiply :: Matrix -> Matrix -> Matrix
multiply (a11,a12,a13,a21,a22,a23,a31,a32,a33) (b11,b12,b13,b21,b22,b23,b31,b32,b33) =
  ( a11*b11 + a12*b21 + a13*b31 , a11*b12 + a12*b22 + a13*b32 , a11*b13 + a12*b23 + a13*b33
  , a21*b11 + a22*b21 + a23*b31 , a21*b12 + a22*b22 + a23*b32 , a21*b13 + a22*b23 + a23*b33
  , a31*b11 + a32*b21 + a33*b31 , a31*b12 + a32*b22 + a33*b32 , a31*b13 + a32*b23 + a33*b33
  )

reduce :: Matrix -> Point -> Point
reduce (a11,a12,a13,a21,a22,a23,a31,a32,a33) (b1,b2,b3) =
  ( a11*b1 + a12*b2 + a13*b3
  , a21*b1 + a22*b2 + a23*b3
  , a31*b1 + a32*b2 + a33*b3
  )

sin0 = 0
sin90 = 1
sin180 = 0
sin270 = -1
cos0 = 1
cos90 = 0
cos180 = -1
cos270 = 0

rx0 = (1,0,0,0,cos0,-sin0,0,sin0,cos0)
rx90 = (1,0,0,0,cos90,-sin90,0,sin90,cos90)
rx180 = (1,0,0,0,cos180,-sin180,0,sin180,cos180)
rx270 = (1,0,0,0,cos270,-sin270,0,sin270,cos270)

ry0 = (cos0,0,sin0,0,1,0,-sin0,0,cos0)
ry90 = (cos90,0,sin90,0,1,0,-sin90,0,cos90)
ry180 = (cos180,0,sin180,0,1,0,-sin180,0,cos180)
ry270 = (cos270,0,sin270,0,1,0,-sin270,0,cos270)

rz0 = (cos0,-sin0,0,sin0,cos0,0,0,0,1)
rz90 = (cos90,-sin90,0,sin90,cos90,0,0,0,1)
rz180 = (cos180,-sin180,0,sin180,cos180,0,0,0,1)
rz270 = (cos270,-sin270,0,sin270,cos270,0,0,0,1)

rotations :: [Matrix]
rotations = Set.toList $ Set.fromList [multiply x (multiply y z) | x <- [rx0,rx90,rx180,rx270], y <- [ry0,ry90,ry180,ry270], z <- [rz0,rz90,rz180,rz270]]

transform :: Set Point -> [Set Point]
transform ps = [Set.map (reduce r) ps | r <- rotations]

getOverlapConfigs :: Set Point -> Set Point -> [(Set Point,Point)]
getOverlapConfigs a b = Set.toList
                      $ Set.fromList
                      $ mapMaybe (\(pa,pb) -> let d = pSub pa pb
                                                  s = Set.size $ Set.intersection a $ Set.map (pAdd d) b
                                              in if s == 12 then Just (b,d) else Nothing
                                 ) [(pa,pb) | pa <- Set.toList a, pb <- Set.toList b]

getAllOverlapConfigs :: Set Point -> Set Point -> [(Set Point,Point)]
getAllOverlapConfigs a = concatMap (getOverlapConfigs a) . transform

getAllAllOverlapConfigs :: (Int,Set Point) -> [(Int,Set Point)] -> Map (Int,Int) [(Set Point,Point)]
getAllAllOverlapConfigs (a,x) ys = Map.fromList $ map (\(b,y) -> ((a,b),getAllOverlapConfigs x y)) ys

--getAllAllAllOverlapConfigs :: [Set Point] -> Map (Int,Int) [(Set Point,Point)]
--getAllAllAllOverlapConfigs xs = Map.unions $ map (\(x,ys) -> getAllAllOverlapConfigs x ys) $ removeEach xs



getOverlap :: Set Point -> Set Point -> Maybe Point
getOverlap a b = (>>= (\(n,p) -> if n == 12 then Just p else Nothing))
               $ maximumMay
               $ map (\(pa,pb) -> let d = pSub pa pb
                                      s = Set.size $ Set.intersection a (Set.map (pAdd d) b)
                                  in (s,d)
                     ) ps
  where ps = [(pa,pb) | pa <- Set.toList a, pb <- Set.toList b]

getOverlaps :: Set Point -> Set Point -> Maybe Point
getOverlaps a b = listToMaybe $ mapMaybe (getOverlap a) (transform b)

getOffsets :: Set Point -> [(Int,Set Point)] -> [(Int,Point)]
getOffsets x ys = mapMaybe (\(i,y) -> (i,) <$> getOverlaps x y) ys

getAll :: [(Int,Set Point)] -> [((Int,Int),Point)]
getAll = concatMap (\((i,x),xs) -> map (\(j,p) -> ((i,j),p)) $ getOffsets x xs) . removeEach

solve1 :: [Set Point] -> String
--solve1 (a:b:_) = show $ (take 1 (Set.toList b)) ++ [head (Set.toList t) | t <- transform (Set.fromList (take 1 (Set.toList b)))] --getOverlaps a b
solve1 (a:b:c:d:e:_) = show b ++ "\n" ++ (show $ getAllOverlapConfigs a b)
--solve1 xs = show $ getAll $ zip [0..] xs

solve2 :: [Set Point] -> String
--solve2 (a:b:_) = unlines $ map show $ sort $ getOverlap' a b
solve2 _ = ""

parseInt :: Parser Int
parseInt = do
  neg <- optionMaybe (char '-')
  d <- many1 digit
  return $ read $ maybe d (const ('-':d)) neg

parseNumLine :: Parser Point
parseNumLine = (,,) <$> (parseInt <* char ',') <*> (parseInt <* char ',') <*> (parseInt <* newline)

parseScanner :: Parser (Set Point)
parseScanner = do
  _ <- string "--- scanner "
  _ <- parseInt
  _ <- string " ---"
  _ <- newline
  Set.fromList <$> manyTill parseNumLine ((() <$ newline) <|> eof)

parseInput :: Parser [Set Point]
parseInput = many1 parseScanner

readInput :: String -> [Set Point]
readInput = either (error . show) id . runParser parseInput () ""

main :: IO ()
--main = interact (show . (solve1 &&& solve2) . readInput)
main = interact (solve1 . readInput)
