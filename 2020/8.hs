import Data.Array (Array, (!), (//), bounds, indices, listArray)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

import Text.Parsec ((<|>), char, digit, many1, string, try, spaces, parse)
import Text.Parsec.String (Parser)

data Op = Acc Int | Jmp Int | Nop Int deriving Show

data Machine = Machine (Array Int Op) Int Int deriving Show

parseN :: Parser Int
parseN = spaces >> ((char '+' >> return 1) <|> (char '-' >> return (-1))) >>= (\i -> many1 digit >>= (\ds -> return (i * read ds)))

parseLine :: Parser Op
parseLine = try (string "acc" >> (Acc <$> parseN)) <|>
            try (string "jmp" >> (Jmp <$> parseN)) <|>
            try (string "nop" >> (Nop <$> parseN))

readLine :: String -> Op
readLine line = case parse parseLine "" line of
                  Left err -> error (show err)
                  Right op -> op

onerun :: Machine -> Machine
onerun (Machine ops acc pc) = case ops ! pc of
                                Acc i -> Machine ops (acc + i) (pc + 1)
                                Jmp i -> Machine ops acc (pc + i)
                                Nop _ -> Machine ops acc (pc + 1)

buildMachine :: [Op] -> Machine
buildMachine ops = Machine (listArray (0, length ops - 1) ops) 0 0

run' :: Set.Set Int -> Machine -> (Bool,Int)
run' seen (Machine ops acc pc) = if Set.member pc seen
                                 then (False,acc)
                                 else case onerun (Machine ops acc pc) of
                                        Machine o a p -> if p > snd (bounds o)
                                                         then (True,a)
                                                         else run' (Set.insert pc seen) (Machine o a p)

run :: Machine -> (Bool,Int)
run = run' Set.empty

solve1 :: Machine -> Int
solve1 = snd . run

swapOne :: Array Int Op -> Int -> Maybe Int
swapOne ops p = case ops ! p of
                  Acc _ -> Nothing
                  Jmp i -> let (r,a) = run (Machine (ops // [(p,Nop i)]) 0 0) in if r then Just a else Nothing
                  Nop i -> let (r,a) = run (Machine (ops // [(p,Jmp i)]) 0 0) in if r then Just a else Nothing

solve2 :: [Op] -> Int
solve2 ops = head $ mapMaybe (swapOne os) $ indices os
  where os = listArray (0, length ops - 1) ops

main :: IO ()
--main = interact (show . solve1 . buildMachine . map readLine . lines)
main = interact (show . solve2 . map readLine . lines)
