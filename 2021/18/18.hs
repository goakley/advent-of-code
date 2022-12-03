import Text.Parsec (between, char, digit, many1, runParser, (<|>))
import Text.Parsec.String (Parser)
import Control.Arrow ((&&&))

data Snail = Regular Int | Pair Snail Snail deriving (Eq)

instance Show Snail where
  show (Regular i) = show i
  show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

addToRegular :: Either Int Int -> Snail -> (Bool,Snail)
addToRegular n (Regular a) = (True,Regular (either (+a) (+a) n))
addToRegular n (Pair a b) = let (ra,a') = addToRegular n a
                                (rb,b') = addToRegular n b
                            in (ra || rb
                               ,either
                                (const $ Pair a' (if ra then b else b'))
                                (const $ Pair (if rb then a else a') b')
                                n)

explode :: Int -> Snail -> (Maybe (Maybe Int,Maybe Int), Snail)
explode _ (Regular n) = (Nothing,Regular n)
explode i (Pair (Regular a) (Regular b)) = if i == 4
                                           then (Just (Just a,Just b),Regular 0)
                                           else (Nothing,Pair (Regular a) (Regular b))
explode i (Pair (Regular a) (Pair b c)) = if i >= 4
                                          then error "impossible structure"
                                          else let (e,r) = explode (i + 1) (Pair b c)
                                               in maybe
                                                  (Nothing,Pair (Regular a) r)
                                                  (\(v,w) -> (Just (Nothing,w), Pair (Regular (maybe a (+a) v)) r))
                                                  e
explode i (Pair (Pair a b) (Regular c)) = if i >= 4
                                          then error "impossible structure"
                                          else let (e,r) = explode (i + 1) (Pair a b)
                                               in maybe
                                                  (Nothing,Pair r (Regular c))
                                                  (\(v,w) -> (Just (v,Nothing),Pair r (Regular (maybe c (+c) w))))
                                                  e
explode i (Pair (Pair a b) (Pair c d)) = if i >= 4
                                         then error "impossible structure"
                                         else let (e1,r1) = explode (i + 1) (Pair a b)
                                                  (e2,r2) = explode (i + 1) (Pair c d)
                                              in maybe
                                                 (maybe
                                                  (Nothing,Pair (Pair a b) (Pair c d))
                                                  (\(v,w) -> maybe
                                                             (Just (v,w),Pair r1 r2)
                                                             (\v' -> let (x,ab) = addToRegular (Right v') (Pair a b)
                                                                     in (Just (if x then Nothing else Just v',w),Pair ab r2))
                                                             v)
                                                  e2)
                                                 (\(v,w) -> maybe
                                                            (Just (v,w),Pair r1 (Pair c d))
                                                            (\w' -> let (x,cd) = addToRegular (Left w') (Pair c d)
                                                                    in (Just (v,if x then Nothing else Just w'),Pair r1 cd))
                                                            w)
                                                 e1

splitNum :: Int -> (Int,Int)
splitNum n = let half = n `div` 2
             in (half, half + (if even n then 0 else 1))

split :: Snail -> (Bool,Snail)
split (Regular a) = if a >= 10
                    then let (l,r) = splitNum a
                         in (True,Pair (Regular l) (Regular r))
                    else (False,Regular a)
split (Pair l r) = let (a1,l') = split l
                       (a2,r') = split r
                   in (a1 || a2, Pair l' (if a1 then r else r'))

reduce' :: Snail -> (Bool,Snail)
reduce' s = let (r,s') = explode 0 s
                (a,s'') = split s
            in maybe (a,s'') (const (True,s')) r

reduce :: Snail -> Snail
reduce s = let (a,s') = reduce' s
           in if a then reduce s' else s'

addSnails :: Snail -> Snail -> Snail
addSnails l r = reduce $ Pair l r

magnitude :: Snail -> Int
magnitude (Regular a) = a
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

solve1 :: [Snail] -> Int
solve1 = magnitude . foldl1 addSnails

solve2 :: [Snail] -> Int
solve2 ss = maximum [magnitude (addSnails x y) | x <- ss, y <- ss]

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseSnailPair :: Parser Snail
parseSnailPair = between (char '[') (char ']')
               $ Pair <$> parseSnail <*> (char ',' *> parseSnail)

parseSnail :: Parser Snail
parseSnail = (Regular <$> parseInt) <|> parseSnailPair

readLine :: String -> Snail
readLine = either (error "unsolvable") id . runParser parseSnail () ""

main :: IO ()
main = interact (show . (solve1 &&& solve2) . map readLine . lines)
