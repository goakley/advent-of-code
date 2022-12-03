rawInput :: String
rawInput = ".^^^.^.^^^^^..^^^..^..^..^^..^.^.^.^^.^^....^.^...^.^^.^^.^^..^^..^.^..^^^.^^...^...^^....^^.^^^^^^^"

input :: [Bool]
input = map (=='^') rawInput

isTrap :: (Bool,Bool,Bool) -> Bool
isTrap (l,c,r) = (l && c && not r) ||
                 (c && r && not l) ||
                 (l && not c && not r) ||
                 (r && not c && not l)

group3 :: [Bool] -> [(Bool,Bool,Bool)]
group3 (a:b:c:xs) = (a,b,c) : group3 (b:c:xs)
group3 _ = []

itr :: [Bool] -> [Bool]
itr xs = map isTrap $ group3 ([False] ++ xs ++ [False])

safes :: Int -> Int
safes n = length $ filter not $ concat $ take n $ iterate itr input

solution1 :: Int
solution1 = safes 40

solution2 :: Int
solution2 = safes 400000

main :: IO ()
main = print solution1 >> print solution2
