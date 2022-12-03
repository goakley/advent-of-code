import Data.Bits
import qualified Data.Map as Map
import Text.Parsec ((<|>), char, digit, many1, parse, spaces, string, try)
import Text.Parsec.String (Parser)

data MC = MX | MZ | MO deriving (Show, Eq)
data Command = Mask [MC] | Mem Int Int deriving (Show)

parseMaskLine :: Parser Command
parseMaskLine = string "mask" >> spaces >> char '=' >> spaces >> (Mask <$> many1 ((char 'X' >> return MX) <|> (char '0'>> return MZ) <|> (char '1' >> return MO)))

parseMemLine :: Parser Command
parseMemLine = do
  _ <- string "mem" >> char '['
  a <- many1 digit
  char ']' >> spaces >> char '=' >> spaces
  b <- many1 digit
  return $ Mem (read a) (read b)

readLine :: String -> Command
readLine line = case parse (try parseMaskLine <|> parseMemLine) "" line of
                  Left err -> error (show err)
                  Right x -> x

applyMask1 :: [MC] -> Int -> Int
applyMask1 xs = zeros . ones
  where
    zeros = foldl (\f (i,_) -> (`clearBit` i) . f) id $ filter ((==MZ) . snd) $ zip [0..] (reverse xs)
    ones = foldl (\f (i,_) -> (`setBit` i) . f) id $ filter ((==MO) . snd) $ zip [0..] (reverse xs)

step1 :: ([MC], Map.Map Int Int) -> Command -> ([MC], Map.Map Int Int)
step1 (_, m) (Mask mask') = (mask', m)
step1 (mask, m) (Mem a v) = (mask, Map.insert a (applyMask1 mask v) m)

run1 :: [Command] -> Map.Map Int Int
run1 = snd . foldl step1 ([],Map.empty)

solve1 :: [Command] -> Int
solve1 = sum . Map.elems . run1

applyMask2 :: [MC] -> Int -> [Int]
applyMask2 xs v = rest
  where pairs = zip [0..] (reverse xs)
        pass = foldl (\a (i,b) -> if b == MZ then a else a `setBit` i) v $ filter ((/=MX) . snd) pairs
        rest = foldl (\as (i,_) -> concatMap (\a -> [a `setBit` i, a `clearBit` i]) as) [pass] $ filter ((==MX) . snd) pairs

step2 :: ([MC], Map.Map Int Int) -> Command -> ([MC], Map.Map Int Int)
step2 (_, m) (Mask mask') = (mask', m)
step2 (mask, m) (Mem a v) = (mask, foldl (\m' n -> Map.insert n v m') m (applyMask2 mask a))

run2 :: [Command] -> Map.Map Int Int
run2 = snd . foldl step2 ([],Map.empty)

solve2 :: [Command] -> Int
solve2 = sum . Map.elems . run2

main :: IO ()
--main = interact (show . solve1 . map readLine . lines)
main = interact (show . solve2 . map readLine . lines)
