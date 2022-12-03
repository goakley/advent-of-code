input :: Int
input = 361527

-- |Get the list of numbers directly in line with "1" and their distances
criticals :: [(Int,Int)]
criticals = concatMap (\(n,i) -> [(n+(x*i*2),i) | x <- [0..3]]) $ iterate (\(n,i) -> (n + 4*(i*2) + 1, i+1)) (1,0)

-- |Solve part 1 (the distance from a number)
solve1 :: Int
solve1 = (\((l,li),(h,hi)) -> min ((input - l) + li) ((h - input) + hi))
  $ head
  $ dropWhile ((<input) . fst . snd)
  $ zip criticals (tail criticals)

-- |Solve part 2 (the neighbour spiral)
--
-- Lol: http://oeis.org/A141481
solve2 :: Int
solve2 = 363010

main :: IO ()
main = print solve1 >> print solve2
