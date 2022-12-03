import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Parsec ((<|>), alphaNum, char, digit, many1, optional, string, try, space, spaces, parse)
import Text.Parsec.String (Parser)

pBagColour :: Parser String
pBagColour = do
  l <- many1 alphaNum
  _ <- space
  r <- many1 alphaNum
  _ <- space
  _ <- string "bag"
  optional (char 's')
  return $ l ++ ' ' : r

pBagCount :: Parser (Int,String)
pBagCount = do
  _ <- spaces
  ds <- many1 digit
  _ <- space
  bc <- pBagColour
  return (read ds, bc)

pBagCounts :: Parser [(Int,String)]
pBagCounts = pBagCount >>= (\bg -> (char ',' >> pBagCounts >>= (\bgs -> return (bg:bgs))) <|>
                                   (char '.' >> return [bg]))

parseLine :: Parser (String,[(Int,String)])
parseLine = do
  bc <- pBagColour
  _ <- string " contain "
  xs <- try nob <|> pBagCounts
  return (bc,xs)
  where
    nob = string "no other bags." >> return []

readLine :: String -> (String,[(Int,String)])
readLine input = case (parse parseLine "" input) of
                      Left err -> error (show err)
                      Right xs -> xs

flipg :: [(String,[(Int,String)])] -> Map.Map String (Set.Set String)
flipg = foldl (\a (k,vs) -> foldl (\aa (_,v) -> Map.alter (Just . maybe (Set.singleton k) (Set.insert k)) v aa) a vs) Map.empty

solve1' :: Map.Map String (Set.Set String) -> [String] -> Set.Set String
solve1' _ [] = Set.empty
solve1' bs (c:cs) = let nexts = Map.findWithDefault Set.empty c bs
                    in Set.insert c $ solve1' (Map.delete c bs) (cs ++ Set.toList nexts)

solve1 :: Map.Map String (Set.Set String) -> Int
solve1 bs = Set.size (solve1' bs ["shiny gold"]) - 1

solve2' :: Map.Map String [(Int,String)] -> String -> Int
solve2' bs c = let nexts = Map.findWithDefault [] c bs
               in sum (map (\(i,n) -> i * (1 + solve2' (Map.delete c bs) n)) nexts)

solve2 :: Map.Map String [(Int,String)] -> Int
solve2 bs = solve2' bs "shiny gold"

showDot :: [(String,[(Int,String)])] -> String
showDot [] = ""
showDot ((k,vs):xs) = concatMap (\(_,v) -> "  \"" ++ k ++ "\" -> \"" ++ v ++ "\"\n") vs ++ showDot xs

main :: IO ()
--main = interact (showDot . map readLine . lines)
--main = interact (show . solve1 . flipg . map readLine . lines)
main = interact (show . solve2 . Map.fromList . map readLine . lines)
