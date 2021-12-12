
-- parse
parse = id


-- solve 1
solve1 = id


-- solve 2
solve2 = id


-- scores
score1 = const 0
score2 = const 0


-- test 
test1a = 0
test2a = 0



-- boiler plate for running

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn $ "solve2: " ++ show (solve2 $ parse input)

  test "1" test1a (score1 $ solve1 $ parse input)
  test "2" test2a (score2 $ solve2 $ parse input)


  return ()

run = do
  input <- readFile "input"
  let a1 = score1 $ solve1 $ parse input
  let a2 = score2 $ solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> run

