import Data.List

solve1 :: [Int] -> Int
solve1 (x:y:xs) = (if x < y then 1 else 0)+ solve1 (y:xs)
solve1 _ = 0

solve2 :: [Int] -> Int
solve2 (a:b:c:d:xs) = (if (a+b+c) < (b+c+d) then 1 else 0)+ solve2 (b:c:d:xs)
solve2 _ = 0

parse = map read . lines

test1a = 0
test2a = 0



-- boiler plate

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "input"
  test "1" test1a (solve1 $ parse input)
  test "2" test2a (solve2 $ parse input)
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = solve1 $ parse input
  let a2 = solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

