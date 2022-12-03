import Control.Arrow (first, (&&&), (***))
import Control.Monad.State
import Data.HashMap.Strict ((!))
import Text.Parsec (digit, eof, many1, newline, runParser, string)
import Text.Parsec.String (Parser)
import qualified Data.HashMap.Strict as Map

-- convenience factor +1
type Map = Map.HashMap

-- |The predictable practice die
newtype Dice = Dice Int deriving Show

-- |Get the practice die at the initial state
newDice :: Dice
newDice = Dice 0

diceRolls :: Dice -> Int
diceRolls (Dice n) = n

-- |Roll the dice the three times needed to generate a move
roll3 :: State Dice Int
roll3 = state ((\(x,d') -> (\(y,d'') -> first ((x + y) +) $ runState roll d'') $ runState roll d') . runState roll)
  where roll = state (\(Dice n) -> ((n `mod` 100) + 1, Dice (n + 1)))

-- |Add two numbers clamped in the range [1,10]
move :: Int -> Int -> Int
move n z = let n' = (n + z) `mod` 10
              in if n' < 1 then 10 else n'

-- |The state of the dice game
data Game = Game Dice (Int,Int) (Int,Int) deriving Show

-- |Create an initial game state from the two player starting positions
newGame :: (Int,Int) -> Game
newGame = Game newDice (0,0)

-- |Play a game (specifying if player one goes first), providing the losing*roll score at the end
play :: Bool -> State Game Int
play l = state (\(Game dice (a,b) (x,y)) -> let (z,dice') = runState roll3 dice
                                                x' = if l then move x z else x
                                                y' = if l then y else move y z
                                                a' = if l then a + x' else a
                                                b' = if l then b else b + y'
                                                game' = Game dice' (a',b') (x',y')
                                            in case (a' >= 1000, b' >= 1000) of
                                                 (True,True)   -> undefined -- both players won???
                                                 (False,False) -> runState (play (not l)) game'
                                                 (True,False)  -> (b' * diceRolls dice', game')
                                                 (False,True)  -> (a' * diceRolls dice', game'))

-- |The count of all possible dirac die rolls (quantum)
diracRolls :: [(Int,Int)]
diracRolls = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

-- |The number of wins for each player at a given (dirac) game state
diracStates :: Map ((Int,Int),(Int,Int),Bool) (Int,Int)
diracStates = foldr (\((a,x),(b,y),t) m' -> Map.insert
                                            ((a,x),(b,y),t)
                                            ((sum . map fst &&& sum . map snd) $ map (\(v,w) -> ((w*) *** (w*)) (m' ! (if t then ((a + move x v,move x v),(b,y),not t) else ((a,x),(b + move y v,move y v),not t)))) diracRolls)
                                            m') m
            $ [((a,x),(b,y),t) | a <- [0..20], b <- [0..20], x <- [1..10], y <- [1..10], t <- [True,False]]
  where m = Map.fromList
          $ [(((a,x),(b,y),t),(0,1)) | a <- [0..20], b <- [21..30], x <- [1..10], y <- [1..10], t <- [True,False]]
         ++ [(((b,y),(a,x),t),(1,0)) | a <- [0..20], b <- [21..30], x <- [1..10], y <- [1..10], t <- [True,False]]

solve1 :: (Int,Int) -> Int
solve1 = evalState (play True) . newGame

solve2 :: (Int,Int) -> Int
solve2 (x,y) = uncurry max $ Map.findWithDefault (error "unsolvable") ((0,x),(0,y),True) diracStates

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseLine :: Int -> Parser Int
parseLine n = string "Player " *> string (show n) *> string " starting position: " *> parseInt <* newline

parseInput :: Parser (Int,Int)
parseInput = ((,) <$> parseLine 1 <*> parseLine 2) <* eof

readInput :: String -> (Int,Int)
readInput = either (error . show) id . runParser parseInput () ""

main :: IO ()
main = interact (show . (solve1 &&& solve2) . readInput)
