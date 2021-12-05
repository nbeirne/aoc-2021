
parse = id

solve1 = id
solve2 = id

-- score a solution based on the id
score = const 0

test1a = 0
test2a = 0

-- boiler plate

test name expected result = do
  putStrLn $ "test "++ name ++ " got "++show result
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  test "1" test1a (score $ solve1 $ parse input)
  test "2" test2a (score $ solve2 $ parse input)

  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn $ "solve2: " ++ show (solve2 $ parse input)

  return ()

solveAll = do
  input <- readFile "input"
  let a1 = score $ solve1 $ parse input
  let a2 = score $ solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

