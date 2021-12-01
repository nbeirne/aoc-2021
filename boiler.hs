
parse = id

solve1 = id
solve2 = id

test1a = ""
test2a = ""

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

