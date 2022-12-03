import Control.Arrow ((&&&))
import Data.Bits
import Data.Char (ord)
import Data.Int (Int8)
import Data.List (partition)
import qualified Data.Map as Map

-- |Split a list into sublists based on a boundary value
-- (the boundary value is not included in the result)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c xs = uncurry (:) $ splitOn c . drop 1 <$> break (==c) xs

newtype Signal = Signal Int8 deriving (Eq, Ord)

instance Show Signal where
  show (Signal s) = concatMap (\(i,c) -> [c | testBit s i]) $ zip [0..] "abcdefg"

-- |Convert the "ab..." input thing to a Signal
wiresToSignal :: String -> Signal
wiresToSignal = Signal . foldl setBit 0 . map (\c -> ord c - 97)

-- |The number of active wires in the Signal
signalSize :: Signal -> Int
signalSize (Signal i) = length $ filter (testBit i) [0..6]

-- |The number of active wires common to both Signals
signalOverlap :: Signal -> Signal -> Int
signalOverlap (Signal a) (Signal b) = signalSize (Signal (a .&. b))

-- |Build a mapping from signals to intended displayed values given observed signals
buildDecoder :: [Signal] -> Map.Map Signal Int
buildDecoder xs = Map.fromList [(zero,0),(one,1),(two,2),(three,3),(four,4),(five,5),(six,6),(seven,7),(eight,8),(nine,9)]
  where
    -- some of these are a given
    [one]   = filter ((==2) . signalSize) xs
    [seven] = filter ((==3) . signalSize) xs
    [four]  = filter ((==4) . signalSize) xs
    [eight] = filter ((==7) . signalSize) xs
    -- the rest, we can group by the number of signals they have
    [ttf1,ttf2,ttf3] = filter ((==5) . signalSize) xs
    [zsn1,zsn2,zsn3] = filter ((==6) . signalSize) xs
    -- and then calculate their overlaps with already-known values to identify each of them
    ([six],[zeronine1,zeronine2]) = partition ((==1) . signalOverlap one) [zsn1, zsn2, zsn3]
    ([nine],[zero])               = partition ((==4) . signalOverlap four) [zeronine1,zeronine2]
    ([three],[twofive1,twofive2]) = partition ((==2) . signalOverlap one) [ttf1,ttf2,ttf3]
    ([five],[two])                = partition ((==5) . signalOverlap six) [twofive1,twofive2]

-- |Decode a jumbled display using a mapping from signals to intended displayed values
decode :: Map.Map Signal Int -> [Signal] -> Int
--decode m = maybe 0 id . foldl (\a s -> a >>= (\d -> ((d * 10) +) <$> (Map.lookup s m))) (Just 0)
decode m = foldl (\a s -> a * 10 + m Map.! s) 0

solve1 :: [([Signal],[Signal])] -> Int
solve1 = sum . map (\(_,ys) -> length $ filter ((`elem` [2,3,4,7]) . signalSize) ys)

solve2 :: [([Signal],[Signal])] -> Int
solve2 xs = sum $ map (uncurry $ decode . buildDecoder) xs

readLine :: String -> ([Signal],[Signal])
readLine xs = (l,r) where [l,r] = map (map wiresToSignal . words) $ splitOn '|' xs

main :: IO ()
main = interact (show . (solve1 &&& solve2) . map readLine . lines)
