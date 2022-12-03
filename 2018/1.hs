import qualified Data.Set

readLine :: String -> Int
readLine ('+':xs) = read xs
readLine ('-':xs) = (-1) * read xs
readLine _ = undefined

readInput :: String -> [Int]
readInput = map readLine . lines

solve1 :: [Int] -> Int
solve1 = sum

solve2 :: Int -> Data.Set.Set Int -> [Int] -> Int
solve2 _ _ [] = undefined
solve2 v s (x:xs) = if Data.Set.member v s
                    then v
                    else solve2 (v + x) (Data.Set.insert v s) xs

main :: IO ()
--main = interact (show . solve1 . readInput)
main = interact (show . solve2 0 (Data.Set.empty) . cycle . readInput)
