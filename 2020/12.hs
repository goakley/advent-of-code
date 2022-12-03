import Debug.Trace (trace)
import Text.Parsec ((<|>), char, digit, many1, parse)
import Text.Parsec.String (Parser)

data Dir = NO | SO | EA | WE
data Ins = N | S | E | W | L | R | F deriving (Eq, Show)

parseIns :: Parser Ins
parseIns = (char 'N' >> return N)
           <|> (char 'S' >> return S)
           <|> (char 'E' >> return E)
           <|> (char 'W' >> return W)
           <|> (char 'L' >> return L)
           <|> (char 'R' >> return R)
           <|> (char 'F' >> return F)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseLine :: Parser (Ins,Int)
parseLine = do
  a <- parseIns
  b <- parseInt
  return (a,b)

readLine :: String -> (Ins,Int)
readLine line = case parse parseLine "" line of
                  Left err -> error (show err)
                  Right x -> x

step1 :: (Ins,Int) -> (Int,Int,Dir) -> (Int,Int,Dir)
step1 (N,n) (x,y,d) = (x + n, y, d)
step1 (S,n) (x,y,d) = (x - n, y, d)
step1 (E,n) (x,y,d) = (x, y + n, d)
step1 (W,n) (x,y,d) = (x, y - n, d)
step1 (L,0) (x,y,d) = (x,y,d)
step1 (R,0) (x,y,d) = (x,y,d)
step1 (L,90) (x,y,NO) = (x,y,WE)
step1 (L,90) (x,y,WE) = (x,y,SO)
step1 (L,90) (x,y,SO) = (x,y,EA)
step1 (L,90) (x,y,EA) = (x,y,NO)
step1 (R,90) (x,y,NO) = (x,y,EA)
step1 (R,90) (x,y,EA) = (x,y,SO)
step1 (R,90) (x,y,SO) = (x,y,WE)
step1 (R,90) (x,y,WE) = (x,y,NO)
step1 (L,180) (x,y,NO) = (x,y,SO)
step1 (L,180) (x,y,SO) = (x,y,NO)
step1 (L,180) (x,y,EA) = (x,y,WE)
step1 (L,180) (x,y,WE) = (x,y,EA)
step1 (R,180) xyd = step1 (L,180) xyd
step1 (L,270) xyd = step1 (R,90) xyd
step1 (R,270) xyd = step1 (L,90) xyd
step1 (L,n) xyd = if n `mod` 90 == 0 then step1 (L,n - 360) xyd else undefined
step1 (R,n) xyd = if n `mod` 90 == 0 then step1 (R,n - 360) xyd else undefined
step1 (F,n) (x,y,NO) = step1 (N,n) (x,y,NO)
step1 (F,n) (x,y,SO) = step1 (S,n) (x,y,SO)
step1 (F,n) (x,y,EA) = step1 (E,n) (x,y,EA)
step1 (F,n) (x,y,WE) = step1 (W,n) (x,y,WE)

solve1 :: [(Ins,Int)] -> Int
solve1 = (\(x,y,_) -> abs x + abs y) . foldl (\(x,y,d) (i,n) -> step1 (i,n) (x,y,d)) (0,0,EA)

step2 :: (Ins,Int) -> ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
step2 (N,n) (xy,(u,v)) = (xy,(u+n,v))
step2 (S,n) (xy,(u,v)) = (xy,(u-n,v))
step2 (E,n) (xy,(u,v)) = (xy,(u,v+n))
step2 (W,n) (xy,(u,v)) = (xy,(u,v-n))
step2 (L,0) xyuv = xyuv
step2 (R,0) xyuv = xyuv
step2 (L,90) (xy,(u,v)) = (xy,(v,-u))
step2 (R,90) (xy,(u,v)) = (xy,(-v,u))
step2 (L,180) (xy,(u,v)) = (xy,(-u,-v))
step2 (R,180) xyuv = step2 (L,180) xyuv
step2 (L,270) xyuv = step2 (R,90) xyuv
step2 (R,270) xyuv = step2 (L,90) xyuv
step2 (L,n) xyuv = if n `mod` 90 == 0 then step2 (L,n - 360) xyuv else undefined
step2 (R,n) xyuv = if n `mod` 90 == 0 then step2 (R,n - 360) xyuv else undefined
step2 (F,n) ((x,y),(u,v)) = ((x+(u*n),y+(v*n)),(u,v))

solve2 :: [(Ins,Int)] -> Int
solve2 = (\((x,y),_) -> abs x + abs y) . foldl (\((x,y),(u,v)) (i,n) -> trace (show ((x,y),(u,v))) step2 (i,n) ((x,y),(u,v))) ((0,0),(1,10))

main :: IO ()
--main = interact (show . solve1 . map readLine . lines)
main = interact (show . solve2 . map readLine . lines)
