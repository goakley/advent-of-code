{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.Ord (comparing)
import Data.List (sortBy)
import GHC.Generics (Generic)
import Text.Parsec (anyChar, char, digit, eof, many, many1, manyTill, newline, optionMaybe, runParser, string, try, (<|>))
import Text.Parsec.String (Parser)

type Point = (Int,Int)

data Space = RL Pod | RH Pod | HLL | HL | HAB | HBC| HCD | HR | HRR deriving (Eq, Generic, Show)

instance Hashable Space

isMyRoom :: Pod -> Space -> Bool
isMyRoom p1 (RH p2) = p1 == p2
isMyRoom p1 (RL p2) = p1 == p2
isMyRoom _ _ = False

data Pod = A | B | C | D deriving (Eq, Generic, Ord, Show)

instance Hashable Pod

type State = ((Space,Space),(Space,Space),(Space,Space),(Space,Space))

--finalState :: State
--finalState = ((R A,R A),(R B,R B),(R C,R C),(R D,R D))

nextStatesA1 :: State -> [State]
nextStatesA1 s@((RL A,a2),(b1,b2),(c1,c2),(d1,d2)) = if isTrappedA s
                                                     then []
                                                     else 

nextStates :: State -> [State]
nextStates s@((a1,a2),(b1,b2),(c1,c2),(d1,d2)) = undefined

parsePod :: Parser Pod
parsePod = (char 'A' $> A) <|> (char 'B' $> B) <|> (char 'C' $> C) <|> (char 'D' $> D)

parseInput :: Parser State
parseInput = do
  _ <- manyTill anyChar newline
  _ <- manyTill anyChar newline
  _ <- string "###"
  a <- (,RH A) <$> parsePod
  _ <- char '#'
  b <- (,RH B) <$> parsePod
  _ <- char '#'
  c <- (,RH C) <$> parsePod
  _ <- char '#'
  d <- (,RH D) <$> parsePod
  _ <- string "###"
  _ <- newline
  _ <- string "  "
  e <- (,RL A) <$> parsePod
  _ <- char '#'
  f <- (,RL B) <$> parsePod
  _ <- char '#'
  g <- (,RL C) <$> parsePod
  _ <- char '#'
  h <- (,RL D) <$> parsePod
  _ <- string "#"
  _ <- newline
  _ <- string "  #########"
  _ <- eof
  let [a,b,c,d,e,f,g,h] = sortBy (comparing (fst :: ((Pod,Space) -> Pod))) [a,b,c,d,e,f,g,h]
  return ((snd a,snd b),(snd c,snd d),(snd e, snd f),(snd g,snd h))
