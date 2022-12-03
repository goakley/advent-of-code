import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Functor (($>))
import Text.Parsec (Parsec, anyToken, char, eof, getPosition, many, runParser, sourceColumn, (<|>))
import Text.Parsec.String (Parser)

type ParserS = Parsec String ()
type ParserB = Parsec [Bool] ()

parseHex' :: ParserS [Bool]
parseHex' = (char '0' $> [False,False,False,False])
        <|> (char '1' $> [False,False,False,True ])
        <|> (char '2' $> [False,False,True ,False])
        <|> (char '3' $> [False,False,True ,True ])
        <|> (char '4' $> [False,True ,False,False])
        <|> (char '5' $> [False,True ,False,True ])
        <|> (char '6' $> [False,True ,True ,False])
        <|> (char '7' $> [False,True ,True ,True ])
        <|> (char '8' $> [True ,False,False,False])
        <|> (char '9' $> [True ,False,False,True ])
        <|> (char 'A' $> [True ,False,True ,False])
        <|> (char 'B' $> [True ,False,True ,True ])
        <|> (char 'C' $> [True ,True ,False,False])
        <|> (char 'D' $> [True ,True ,False,True ])
        <|> (char 'E' $> [True ,True ,True ,False])
        <|> (char 'F' $> [True ,True ,True ,True ])

parseHex :: ParserS [Bool]
parseHex = concat <$> many parseHex' <* eof

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
-- whatever
hexToBin _   = ""

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = 0

binToInt :: [Bool] -> Int
binToInt = foldl (\a b -> a * 2 + (if b then 1 else 0)) 0

parse01 :: Parser Bool
parse01 = (char '0' $> False) <|> (char '1' $> True)

parseVersion :: Parser Int
parseVersion = binToInt <$> replicateM 3 parse01

type Operation = ([Int] -> Int)

data Type = TLiteral | TOperation Operation

parseType :: Parser Type
parseType = do
  a <- parse01
  b <- parse01
  c <- parse01
  return $ case (a,b,c) of
    (False,False,False) -> TOperation sum
    (False,False,True ) -> TOperation product
    (False,True, False) -> TOperation minimum
    (False,True, True ) -> TOperation maximum
    (True, False,False) -> TLiteral
    (True, False,True ) -> TOperation $ boolToInt . all (uncurry (>)) . uncurry zip . (init &&& tail)
    (True, True, False) -> TOperation $ boolToInt . all (uncurry (<)) . uncurry zip . (init &&& tail)
    (True, True, True ) -> TOperation $ boolToInt . all (uncurry (==)) . uncurry zip . (init &&& tail)

parseLiteralChunk :: Parser [Int]
parseLiteralChunk = do
  a   <- parse01
  val <- binToInt <$> replicateM 4 parse01
  if a
    then (val:) <$> parseLiteralChunk
    else return [val]

parseLiteral :: Parser Int
parseLiteral = foldl (\a b -> a * 16 + b) 0 <$> parseLiteralChunk

data Length = Size Int | Count Int

parseLength :: Parser Length
parseLength = do
  i <- parse01
  if i
    then Count . binToInt <$> replicateM 11 parse01
    else Size . binToInt <$> replicateM 15 parse01

data Packet = Literal Int | Operator Operation [(Int,Packet)]

parsePacketsBySize :: Int -> Parser [(Int,Packet)]
parsePacketsBySize 0 = return []
parsePacketsBySize i = do
  col1 <- sourceColumn <$> getPosition
  p    <- parsePacket
  col2 <- sourceColumn <$> getPosition
  (p:) <$> parsePacketsBySize (i - (col2 - col1))

parsePacket :: Parser (Int,Packet)
parsePacket = do
  version <- parseVersion
  typ     <- parseType
  (\x -> (version,x))
    <$> (case typ of
           TLiteral     -> Literal <$> parseLiteral
           TOperation f -> Operator f
                       <$> (parseLength >>= (\l -> case l of
                                                     Size i -> parsePacketsBySize i
                                                     Count n -> replicateM n parsePacket)))

parseInput :: Parser (Int,Packet)
parseInput = parsePacket <* (many (char '0') *> eof)

solve1 :: (Int,Packet) -> Int
solve1 (v,Literal _) = v
solve1 (v,Operator _ ps) = (+v) $ sum $ map solve1 ps

solve2 :: (Int,Packet) -> Int
solve2 (_,Literal x) = x
solve2 (_,Operator f ps) = f $ map solve2 ps

readInput :: String -> (Int,Packet)
readInput = either (error "unsovlable") id . runParser parseInput () "" . concatMap hexToBin

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
