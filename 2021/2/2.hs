import Control.Arrow ((&&&))

data Direction = Forward | Down | Up

readLine :: String -> (Direction, Int)
readLine s = (direction, read i)
  where
    [d, i] = words s
    direction = case d of
      "forward" -> Forward
      "down" -> Down
      "up" -> Up
      _ -> undefined

step1 :: (Int, Int) -> (Direction, Int) -> (Int, Int)
step1 (position, depth) (Forward, i) = (position + i, depth)
step1 (position, depth) (Down, i) = (position, depth + i)
step1 (position, depth) (Up, i) = (position, depth - i)

solve1 :: [(Direction, Int)] -> Int
solve1 = uncurry (*) . foldl step1 (0, 0)

step2 :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
step2 (position, depth, aim) (Forward, i) = (position + i, depth + aim * i, aim)
step2 (position, depth, aim) (Down, i) = (position, depth, aim + i)
step2 (position, depth, aim) (Up, i) = (position, depth, aim - i)

solve2 :: [(Direction, Int)] -> Int
solve2 = (\(p, d, _) -> p * d) . foldl step2 (0, 0, 0)

main :: IO ()
main = interact (show . (solve1 &&& solve2) . map readLine . lines)
