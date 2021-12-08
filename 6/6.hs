import Control.Monad

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'

change :: Int -> (a -> a) -> [a] -> [a]
change i f xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs

-- parse list into a list of counts. idx is the day, value is count.
parse :: String -> [Int]
parse = flipArr . map read . splitBy (==',')
  where flipArr = foldl (\xs x -> change x (+1) xs) [0,0,0,0,0,0,0,0,0] -- 0-8


-- run 

runDay :: [Int] -> [Int]
runDay (x:xs) =  -- x are 0-counts, xs is the rest. 
  (change 6 (+x) xs) ++ [x] -- add x at 6 point, replecate at the end too
    
runDays :: Int -> [Int] -> [Int]
runDays n l = head $ drop n $ iterate runDay l

solve1 = runDays 80
solve2 = runDays 256

-- score by summing
score = sum

test1a = 5934
test2a = 26984457539

-- boiler plate

test name expected result = do
  putStrLn $ "test "++ name ++ " got "++show result
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  --putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  --putStrLn $ "solve2: " ++ show (solve2 $ parse input)

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

