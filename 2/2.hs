
data Ins = Forward Int | Down Int | Up Int | NoOp

parse = map parseLine . lines
parseLine l = case wd !! 0 of 
            "forward" -> Forward n
            "down" -> Down n
            "up" -> Up n
            _ -> NoOp
  where wd = words l
        n = ((read (wd !! 1)) :: Int)

solve1 = m . foldr run (0,0)
  where run (Forward n) (p,d) = (p+n,d)
        run (Up n) (p,d) = (p,d-n)
        run (Down n) (p,d) = (p,d+n)
        m (p,d) = p*d

solve2 = m . foldl run (0,0,0)
  where run (p,d,a) (Forward n) = (p+n,d+(n*a),a)
        run (p,d,a) (Up n) = (p,d,a-n)
        run (p,d,a) (Down n) = (p,d,a+n)
        m (p,d,_) = p*d

test1a = 150
test2a = 900

-- boiler plate

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
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

