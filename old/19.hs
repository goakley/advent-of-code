input :: Int
input = 3004953

solve1 :: Int -> Int
solve1 n = (n - (2 ^ (floor (logBase (2::Double) (fromIntegral n))))) * 2 + 1

solution1 :: Int
solution1 = solve1 input

circ :: Int -> Int
circ 1 = 1
circ n = let d = (n - s) + 1 in if d <= c then d else 2 * d - c
  where s = (3 ^ (floor (logBase 3 (3 * (fromIntegral (n::Int) :: Double) - 3)) - 1)) + 1 -- A094388
        c = s - 1

solution2 :: Int
solution2 = circ input

main :: IO ()
main = print solution1 >> print solution2
