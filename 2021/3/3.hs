import Control.Arrow ((&&&))
import Data.Array (Array, bounds, listArray, (!))
import Data.Maybe (listToMaybe, mapMaybe)

type Bit = Bool -- close enough: 1=True 0=False

-- |Find the most common value in a list,
-- specifically for `Bit`s since we want a tie to favour `1` (`True`)
-- which isn't really generalisable to other types
mostCommon :: [Bit] -> Bit
mostCommon = uncurry (<=) . foldl (\(z,o) n -> if n then (z,o+1) else (z+1,o)) (0,0::Int)

-- |Find the least common value in a list
leastCommon :: [Bit] -> Bit
leastCommon = not . mostCommon

-- |Convert the binary representation of a positive number to an `Int`
binToInt :: [Bit] -> Int
binToInt = foldl (\a b -> (a * 2) + (if b then 1 else 0)) 0

-- |Turn our rows of binary numbers into a 2d matrix for quick lookup
readInput :: String -> Array (Int, Int) Bit
readInput s = listArray ((1, 1), (xsize, ysize)) elements
  where
    ss = lines s
    xsize = length ss
    ysize = maybe 0 length $ listToMaybe ss
    elements = map (/='0') $ concat ss

-- |Get the integer represented by the most common bits in each column
mostCommonBits :: Array (Int, Int) Bit -> Int
mostCommonBits a = binToInt $ map f [ymin..ymax]
  where ((xmin,ymin),(xmax,ymax)) = bounds a
        f z = mostCommon $ map (\x -> a ! (x,z)) [xmin..xmax]

solve1 :: Array (Int, Int) Bit -> Int
solve1 a = gamma * epsilon
  where 
    gamma = mostCommonBits a
    epsilon = mostCommonBits (fmap not a) -- lol just flip the bits to get the "least" common

-- |Get the integer represented by the binary determined by that crazy reduction algorithm
commonValue :: ([Bit] -> Bit) -> Array (Int, Int) Bit -> Int
commonValue fn a = binToInt
                 $ maybe (error "could not reduce list to one value") (\x -> map (\y -> a ! (x,y)) [ymin..ymax])
                 $ listToMaybe -- grab the very first reduction that has only one value left
                 $ mapMaybe (\(_,vals) -> case vals of
                                [val] -> Just val
                                _     -> Nothing)
                 $ iterate (\(y,xs) -> let t = fn (map (\x -> a ! (x,y)) xs)
                                       in (y + 1, filter (\x -> (a ! (x,y)) == t) xs))
                 (ymin,[xmin..xmax]) -- iterate from the leftmost index, filtering out the indices of values as we go
  where
    ((xmin,ymin),(xmax,ymax)) = bounds a

solve2 :: Array (Int, Int) Bit -> Int
solve2 a = oxygen * co2
  where
    oxygen = commonValue mostCommon a
    co2 = commonValue leastCommon a

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
