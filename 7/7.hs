
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'

parse :: String -> [Int]
parse = map read . splitBy (==',')


totalFuel p = sum . map (abs . (-)p)
solve1 xs = minimum $ map (flip totalFuel xs) [0..(maximum xs)]

totalFuel' p = sum . map (cost . distance p)
  where distance a b = abs (a-b)
        cost n = (n * (n+1)) `div` 2
solve2 xs = minimum $ map (flip totalFuel' xs) [1..(maximum xs)]


-- score a solution based on the id
score = id

test1a = 37
test2a = 168

-- boiler plate

test name expected result = do
  putStrLn $ "test "++ name ++ " got "++show result
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  let input = "16,1,2,0,4,2,7,1,2,14" 
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn $ "solve2: " ++ show (solve2 $ parse input)

  test "1" test1a (score $ solve1 $ parse input)
  test "2" test2a (score $ solve2 $ parse input)


  return ()

solveAll = do
  input <- readFile "input"
  let a1 = score $ solve1 $ parse input
  let a2 = score $ solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

