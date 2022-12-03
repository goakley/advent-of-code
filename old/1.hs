import Data.List (tails)

rawInput :: String
rawInput = "R3, R1, R4, L4, R3, R1, R1, L3, L5, L5, L3, R1, R4, L2, L1, R3, L3, R2, R1, R1, L5, L2, L1, R2, L4, R1, L2, L4, R2, R2, L2, L4, L3, R1, R4, R3, L1, R1, L5, R4, L2, R185, L2, R4, R49, L3, L4, R5, R1, R1, L1, L1, R2, L1, L4, R4, R5, R4, L3, L5, R1, R71, L1, R1, R186, L5, L2, R5, R4, R1, L5, L2, R3, R2, R5, R5, R4, R1, R4, R2, L1, R4, L1, L4, L5, L4, R4, R5, R1, L2, L4, L1, L5, L3, L5, R2, L5, R4, L4, R3, R3, R1, R4, L1, L2, R2, L1, R4, R2, R2, R5, R2, R5, L1, R1, L4, R5, R4, R2, R4, L5, R3, R2, R5, R3, L3, L5, L4, L3, L2, L2, R3, R2, L1, L1, L5, R1, L3, R3, R4, R5, L3, L5, R1, L3, L5, L5, L2, R1, L3, L1, L3, R4, L1, R3, L2, L2, R3, R3, R4, R4, R1, L4, R1, L5"

input :: [Either Int Int]
input = map (\(d:ns) -> (if d == 'L' then Left else Right) $ read (if last ns == ',' then init ns else ns)) $ words rawInput

data Dir = N | E | S | W deriving Show

move :: (Dir,(Int,Int)) -> Either Int Int -> (Dir,(Int,Int))
move (N,(x,y)) (Left n) = (W,(x-n,y))
move (N,(x,y)) (Right n) = (E,(x+n,y))
move (E,(x,y)) (Left n) = (N,(x,y+n))
move (E,(x,y)) (Right n) = (S,(x,y-n))
move (S,(x,y)) (Left n) = (E,(x+n,y))
move (S,(x,y)) (Right n) = (W,(x-n,y))
move (W,(x,y)) (Left n) = (S,(x,y-n))
move (W,(x,y)) (Right n) = (N,(x,y+n))

solution1 :: Int
solution1 = (\(_,(a,b)) -> abs a + abs b) $
            foldl move (N,(0,0)) input

positions :: (Dir,(Int,Int)) -> Either Int Int -> [(Dir,(Int,Int))]
positions (N,(x,y)) (Left n) = [(W,(z,y)) | z <- reverse [x-n..x]]
positions (N,(x,y)) (Right n) = [(E,(z,y)) | z <- [x..x+n]]
positions (E,(x,y)) (Left n) = [(N,(x,z)) | z <- [y..y+n]]
positions (E,(x,y)) (Right n) = [(S,(x,z)) | z <- reverse [y-n..y]]
positions (S,(x,y)) (Left n) = [(E,(z,y)) | z <- [x..x+n]]
positions (S,(x,y)) (Right n) = [(W,(z,y)) | z <- reverse [x-n..x]]
positions (W,(x,y)) (Left n) = [(S,(x,z)) | z <- reverse [y-n..y]]
positions (W,(x,y)) (Right n) = [(N,(x,z)) | z <- [y..y+n]]

solution2 :: Int
solution2 = (\(a,b) -> abs a + abs b) $
            head $
            map head $
            filter (\(s:ss) -> s `elem` ss) $
            tails $
            map snd $
            reverse $
            foldl (\(s:ss) e -> reverse (tail (positions s e)) ++ (s : ss)) [(N,(0,0))] input

main :: IO ()
main = print solution1 >> print solution2
