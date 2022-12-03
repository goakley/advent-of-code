import Data.Functor (($>))
import qualified Data.HashSet as Set
import Text.Parsec (char, digit, eof, many, many1, newline, optionMaybe, runParser, string, try, (<|>))
import Text.Parsec.String (Parser)

-- convenience factor +1
type Set = Set.HashSet

data Var = W | X | Y | Z

data Val = V Var | C Int

data Opr = Add | Mul | Div | Mod | Eql

data Command = Input Var | Operate Opr Var Val

type OpState = (Set Int,Set Int,Set Int,Set Int)

data SVal = Inp Int
          | Con Int
          | SAdd SVal SVal
          | SMul SVal SVal
          | SDiv SVal SVal
          | SMod SVal SVal
          | SEql SVal SVal
          deriving (Eq, Show)

type SState = (Int,(SVal,SVal,SVal,SVal))

newSState :: SState
newSState = (0,(Con 0,Con 0,Con 0,Con 0))

reduce :: SVal -> SVal
--
reduce (Inp a) = Inp a
reduce (Con a) = Con a
--
reduce (SAdd a (Con 0)) = a
reduce (SAdd (Con 0) a) = a
reduce (SAdd (Con a) (Con b)) = Con (a + b)
reduce (SAdd (Con a) (SAdd (Con b) c)) = reduce $ SAdd (Con (a + b)) c
reduce (SAdd (Con a) (SAdd b (Con c))) = reduce $ SAdd (Con (a + c)) b
reduce (SAdd (SAdd (Con b) c) (Con a)) = reduce $ SAdd (Con (a + b)) c
reduce (SAdd (SAdd b (Con c)) (Con a)) = reduce $ SAdd (Con (a + c)) b
reduce (SAdd a b) = let a' = reduce a
                        b' = reduce b
                    in if a' == b' then SMul (Con 2) a' else SAdd a' b'
--
reduce (SMul _ (Con 0)) = Con 0
reduce (SMul (Con 0) _) = Con 0
reduce (SMul a (Con 1)) = reduce a
reduce (SMul (Con 1) a) = reduce a
reduce (SMul (Con a) (Con b)) = Con (a * b)
reduce (SMul (Con a) (SMul (Con b) c)) = reduce $ SMul (Con (a * b)) c
reduce (SMul (Con a) (SMul c (Con b))) = reduce $ SMul (Con (a * b)) c
reduce (SMul (SMul (Con b) c) (Con a)) = reduce $ SMul (Con (a * b)) c
reduce (SMul (SMul c (Con b)) (Con a)) = reduce $ SMul (Con (a * b)) c
reduce (SMul a b) = SMul (reduce a) (reduce b)
--
reduce (SDiv _ (Con 0)) = error "DB0"
reduce (SDiv a (Con 1)) = reduce a
reduce (SDiv (Con a) (Con b)) = reduce $ Con (a `div `b) 
reduce (SDiv (SDiv a (Con b)) (Con c)) = reduce $ SDiv a (Con (b * c))
reduce (SDiv a b) = SDiv (reduce a) (reduce b)
--
reduce (SMod (Con 0) _) = Con 0
reduce (SMod _ (Con 1)) = Con 0
reduce (SMod (Con a) (Con b)) = if a < 0 then error "MAN" else if b <= 0 then error "MBN" else Con (mod a b)
reduce (SMod (Inp a) (Con b)) = if b <= 0 then error "MBN" else if b < 10 then SMod (Inp a) (Con b) else Inp a
reduce s@(SMod (SAdd (Inp a) (Con b)) (Con c)) = if (9 + b) < c then SAdd (Inp a) (Con b) else s
reduce s@(SMod (SAdd (Con b) (Inp a)) (Con c)) = if (9 + b) < c then SAdd (Inp a) (Con b) else s
reduce (SMod a b) = SMod (reduce a) (reduce b)
--
reduce (SEql (Con a) (Con b)) = Con (if a == b then 1 else 0)
reduce (SEql (Inp a) (Con b)) = if b < 1 || b > 9 then Con 0 else SEql (Inp a) (Con b)
reduce (SEql (Con a) (Inp b)) = if a < 1 || a > 9 then Con 0 else SEql (Con a) (Inp b)
reduce (SEql (SAdd (Con a) (Inp b)) (Inp 1)





reduce (SEql a b) = SEql (reduce a) (reduce b)

--walk :: (Int -> Int) -> SState -> SState
--walk fn (Inp a) = 

sSelect :: Var -> SState -> SVal
sSelect W (_,(w,_,_,_)) = w
sSelect X (_,(_,x,_,_)) = x
sSelect Y (_,(_,_,y,_)) = y
sSelect Z (_,(_,_,_,z)) = z

sApplyTo :: (SVal -> SVal -> SVal) -> Var -> Val -> SState -> SState
sApplyTo fn W (V a) s@(c,(w,x,y,z)) = (c,(fn w (sSelect a s),x,y,z))
sApplyTo fn W (C a) (c,(w,x,y,z))   = (c,(fn w (Con a),x,y,z))
sApplyTo fn X (V a) s@(c,(w,x,y,z)) = (c,(w,fn x (sSelect a s),y,z))
sApplyTo fn X (C a) (c,(w,x,y,z))   = (c,(w,fn x (Con a),y,z))
sApplyTo fn Y (V a) s@(c,(w,x,y,z)) = (c,(w,x,fn y (sSelect a s),z))
sApplyTo fn Y (C a) (c,(w,x,y,z))   = (c,(w,x,fn y (Con a),z))
sApplyTo fn Z (V a) s@(c,(w,x,y,z)) = (c,(w,x,y,fn z (sSelect a s)))
sApplyTo fn Z (C a) (c,(w,x,y,z))   = (c,(w,x,y,fn z (Con a)))

sApply :: Command -> SState -> SState
--
sApply (Input W) (c,(_,x,y,z)) = (c+1,(Inp c,x,y,z))
sApply (Input X) (c,(w,_,y,z)) = (c+1,(w,Inp c,y,z))
sApply (Input Y) (c,(w,x,_,z)) = (c+1,(w,x,Inp c,z))
sApply (Input Z) (c,(w,x,y,_)) = (c+1,(w,x,y,Inp c))
--
sApply (Operate Add a b) (c,(w,x,y,z)) = sApplyTo (\m -> reduce . SAdd m) a b (c,(w,x,y,z))
sApply (Operate Mul a b) (c,(w,x,y,z)) = sApplyTo (\m -> reduce . SMul m) a b (c,(w,x,y,z))
sApply (Operate Div a b) (c,(w,x,y,z)) = sApplyTo (\m -> reduce . SDiv m) a b (c,(w,x,y,z))
sApply (Operate Mod a b) (c,(w,x,y,z)) = sApplyTo (\m -> reduce . SMod m) a b (c,(w,x,y,z))
sApply (Operate Eql a b) (c,(w,x,y,z)) = sApplyTo (\m -> reduce . SEql m) a b (c,(w,x,y,z))

equation :: Opr -> (Int -> Int -> Int)
equation Add = (+)
equation Mul = (*)
equation Div = (\a b -> if b == 0 then error "DB0" else div a b)
equation Mod = (\a b -> if a < 0 then error "MAN" else if b <= 0 then error "MBN" else mod a b)
equation Eql = (\a b -> if a == b then 1 else 0)

runEquation :: Opr -> Set Int -> Set Int -> Set Int
runEquation o xs ys = Set.fromList [(equation o) x y | x <- Set.toList xs, y <- Set.toList ys]

val2int :: Val -> OpState -> Set Int
val2int (C c) _ = Set.singleton c
val2int (V W) (w,_,_,_) = w
val2int (V X) (_,x,_,_) = x
val2int (V Y) (_,_,y,_) = y
val2int (V Z) (_,_,_,z) = z

executeOpr :: Opr -> Var -> Val -> OpState -> OpState
executeOpr o W v s@(w,x,y,z) = (runEquation o w (val2int v s),x,y,z)
executeOpr o X v s@(w,x,y,z) = (w,runEquation o x (val2int v s),y,z)
executeOpr o Y v s@(w,x,y,z) = (w,x,runEquation o y (val2int v s),z)
executeOpr o Z v s@(w,x,y,z) = (w,x,y,runEquation o z (val2int v s))

newOpState :: OpState
newOpState = let r = Set.fromList [0] in (r,r,r,r)

runCommand :: Command -> OpState -> OpState
runCommand (Input W) (_,x,y,z) = (Set.fromList [1..9],x,y,z)
runCommand (Input X) (w,_,y,z) = (w,Set.fromList [1..9],y,z)
runCommand (Input Y) (w,x,_,z) = (w,x,Set.fromList [1..9],z)
runCommand (Input Z) (w,x,y,_) = (w,x,y,Set.fromList [1..9])
runCommand (Operate o var val) s = executeOpr o var val s

parseVar :: Parser Var
parseVar = (char 'w' $> W) <|> (char 'x' $> X) <|> (char 'y' $> Y) <|> (char 'z' $> Z)

parseInt :: Parser Int
parseInt = do
  neg <- optionMaybe (char '-')
  d <- many1 digit
  return $ read $ maybe d (const ('-':d)) neg

parseVal :: Parser Val
parseVal = (V <$> parseVar) <|> (C <$> parseInt)

parseOpr :: Parser Opr
parseOpr = try (string "add" $> Add) <|> try (string "mul" $> Mul) <|> try (string "div" $> Div) <|> try (string "mod" $> Mod) <|> (string "eql" $> Eql)

parseCommandInput :: Parser Command
parseCommandInput = Input <$> (string "inp " *> parseVar)

parseCommandOperate :: Parser Command
parseCommandOperate = Operate <$> (parseOpr <* char ' ') <*> (parseVar <* char ' ') <*> parseVal

parseCommand :: Parser Command
parseCommand = try parseCommandInput <|> parseCommandOperate

parseLine :: Parser Command
parseLine = parseCommand <* newline

parseInput :: Parser [Command]
parseInput = many parseLine <* eof

readInput :: String -> [Command]
readInput = either (error . show) id . runParser parseInput () ""

main :: IO ()
--main = interact (show . (\(w,x,y,z) -> x) . foldl (flip runCommand) newOpState . readInput)
main = interact ((\(_,(w,x,y,z)) -> show w ++ "\n" ++ show x ++ "\n" ++ show y ++ "\n" ++ show z ++ "\n") . foldl (flip sApply) newSState . readInput)
