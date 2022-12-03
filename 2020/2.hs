import Text.Regex.TDFA

readLine :: String -> (((Int,Int),Char),String)
readLine input = (((read l, read h), head c), p)
  where pat = "([0-9]+)-([0-9]+) (.): (.+)$"
        [[_,l,h,c,p]] = input =~ pat

solve1 :: [(((Int,Int),Char),String)] -> Int
solve1 = length . filter (\(((low,high),c),password) -> let cnt = length (filter (==c) password) in cnt >= low && cnt <= high)

solve2 :: [(((Int,Int),Char),String)] -> Int
solve2 = length . filter (\(((a,b),c),password) -> (password !! (a-1)) /= (password !! (b-1)) && ((password !! (a-1) == c) || (password !! (b-1) == c)))

main :: IO ()
--main = interact (show . solve1 . map readLine . lines)
main = interact (show . solve2 . map readLine . lines)
