import Data.Map (Map, empty, findWithDefault, insert, insertWith, singleton)
import Data.Array (Array, bounds, listArray, (!))

import Text.Regex.TDFA ((=~))

rawInput :: String
rawInput = unlines [
  "cpy 1 a",
  "cpy 1 b",
  "cpy 26 d",
  "jnz c 2",
  "jnz 1 5",
  "cpy 7 c",
  "inc d",
  "dec c",
  "jnz c -2",
  "cpy a c",
  "inc a",
  "dec b",
  "jnz b -2",
  "cpy c b",
  "dec d",
  "jnz d -6",
  "cpy 18 c",
  "cpy 11 d",
  "inc a",
  "dec d",
  "jnz d -2",
  "dec c",
  "jnz c -5"]

data Register = RA | RB | RC | RD deriving (Eq, Ord)

instance Read Register where
  readsPrec _ ('a':xs) = [(RA,xs)]
  readsPrec _ ('b':xs) = [(RB,xs)]
  readsPrec _ ('c':xs) = [(RC,xs)]
  readsPrec _ ('d':xs) = [(RD,xs)]
  readsPrec _ _ = []

data Instruction = Copy (Either Int Register) Register
                 | Increment Register
                 | Decrement Register 
                 | Jump (Either Int Register) Int

readLine :: String -> Instruction
readLine s = case concat (concat [cpy,inc,dec,jnz]) of
              [_,"cpy",a,b] -> Copy (if a `elem` ["a","b","c","d"] then Right (read a) else Left (read a)) (read b)
              [_,"inc",a] -> Increment (read a)
              [_,"dec",a] -> Decrement (read a)
              [_,"jnz",a,b] -> Jump (if a `elem` ["a","b","c","d"] then Right (read a) else Left (read a)) (read b)
              _ -> error (show jnz)
  where cpy = s =~ "(cpy) (-?[a-z0-9]+) ([a-z]+)"
        inc = s =~ "(inc) ([a-z]+)"
        dec = s =~ "(dec) ([a-z]+)"
        jnz = s =~ "(jnz) (-?[a-z0-9]+) (-?[0-9]+)"

input :: Array Int Instruction
input = (\is -> listArray (1,length is) is) $ map readLine $ lines rawInput

type State = (Int, Array Int Instruction, Map Register Int)

step :: State -> State
step (p, is, s) = if p > snd (bounds is)
                  then (p, is, s)
                  else case is Data.Array.! p of
                        (Copy (Left i) r) -> (p + 1, is, Data.Map.insert r i s)
                        (Copy (Right r') r) -> (p + 1, is, Data.Map.insert r (Data.Map.findWithDefault 0 r' s) s)
                        (Increment r) -> (p + 1, is, Data.Map.insertWith (+) r 1 s)
                        (Decrement r) -> (p + 1, is, Data.Map.insertWith (+) r (-1) s)
                        (Jump (Left v) i) -> (if v == 0 then p + 1 else p + i, is, s)
                        (Jump (Right r) i) -> (if Data.Map.findWithDefault 0 r s == 0 then p + 1 else p + i, is, s)

solve :: Map Register Int -> Int
solve state = (\(_,_,s) -> Data.Map.findWithDefault 0 RA s) $ head $ dropWhile (\(p,is,_) -> p <= snd (bounds is)) $ iterate step (1, input, state)

solution1 :: Int
solution1 = solve empty

solution2 :: Int
solution2 = solve (singleton RC 1)

main :: IO ()
main = print solution1 >> print solution2
