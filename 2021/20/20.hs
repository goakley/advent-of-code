import Data.Functor (($>))
import Prelude hiding (lookup)
import Text.Parsec (char, eof, many, newline, runParser, (<|>))
import Text.Parsec.String (Parser)
import Control.Arrow ((&&&))
import qualified Data.HashSet as Set

-- convenience factor +1
type Set = Set.HashSet

type Image = (Bool,((Int,Int),(Int,Int)),Set (Int,Int))

-- |Convert bits into a number
binToInt :: [Bool] -> Int
binToInt = foldl (\a b -> a * 2 + (if b then 1 else 0)) 0

-- |Get all the points adjacent to a given point (including itself)
adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1]]

-- |Get the minimum and maximum x and y bounds of an (x,y) set
setBounds :: Set (Int,Int) -> ((Int,Int),(Int,Int))
setBounds = let minmax = minimum &&& maximum
            in ((minmax . map fst) &&& (minmax . map snd)) . Set.toList

-- |Get the value of a single pixel on the next iteration
getPixelValue :: (Int -> Bool) -> Image -> (Int,Int) -> Bool
getPixelValue lookup (filler,((xmin,xmax),(ymin,ymax)),points) = lookup
                                                               . binToInt
                                                               . map (\(x,y) -> if x >= xmin && x <= xmax && y >= ymin && y <= ymax
                                                                                then Set.member (x,y) points
                                                                                else filler)
                                                               . adjacent

-- |Generate the next iteration of an image
enhanceImage :: (Int -> Bool) -> Image -> Image
enhanceImage lookup image@(filler,((xmin,xmax),(ymin,ymax)),_) = (filler',((xmin',xmax'),(ymin',ymax')),points')
  where ((xmin',xmax'),(ymin',ymax')) = ((xmin-2,xmax+2),(ymin-2,ymax+2))
        filler' = lookup (if filler then 511 else 0)
        points' = Set.fromList
                $ filter (getPixelValue lookup image)
                [(x,y) | x <- [xmin'..xmax'], y <- [ymin'..ymax']]

-- |Make the image back into a pretty picture
showImage :: Image -> String
showImage (filler,((xmin,xmax),(ymin,ymax)),points) = if filler
                                                      then error "infinite"
                                                      else concatMap (\x -> foldr (\y -> ((if Set.member (x,y) points then '#' else '.') :)) "\n" [ymin..ymax]) [xmin..xmax]

-- |Determine how many pixels are lit in an image after `n` enhancements
lightsAfterIter :: Int -> (Int -> Bool) -> Image -> Int
lightsAfterIter n lookup = (\(filler,_,points) -> if filler then error "infinite" else Set.size points) . (!!n) . iterate (enhanceImage lookup)

solve1 :: (Int -> Bool, Image) -> Int
solve1 = uncurry (lightsAfterIter 2)

solve2 :: (Int -> Bool, Image) -> Int
solve2 = uncurry (lightsAfterIter 50)

parsePixel :: Parser Bool
parsePixel = (char '.' $> False) <|> (char '#' $> True)

parsePixelLine :: Parser [Bool]
parsePixelLine = many parsePixel <* newline

parsePixelAlgo :: Parser (Int -> Bool)
parsePixelAlgo = flip Set.member
               . Set.fromList
               . map fst
               . filter snd
               . zip [0..]
               <$> parsePixelLine

parsePixelImage :: Parser Image
parsePixelImage = (\s -> (False,setBounds s,s))
                . Set.fromList
                . map fst
                . filter snd
                . concat
                . zipWith (\x l -> zipWith (\y c -> ((x,y),c)) [1..] l) [1..]
                <$> many parsePixelLine

parseInput :: Parser (Int -> Bool, Image)
parseInput = (,) <$> (parsePixelAlgo <* newline) <*> (parsePixelImage <* eof)

readInput :: String -> (Int -> Bool, Image)
readInput = either (error . show) id . runParser parseInput () ""

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
