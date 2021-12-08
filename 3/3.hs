import Data.List
parse = lines


-- helpers

fromBinary :: Num a => String -> a
fromBinary = conv 0 . reverse
  where conv n ('0':xs) = conv (n+1) xs
        conv n ('1':xs) = (2^n) + conv (n+1) xs
        conv n [] = 0

-- counts elements in array and outputs a list of Count,Element. Each element appears once and order is undefined.
countElements :: Ord a => [a] -> [(Int,a)]
countElements = map (\l -> (length l, head l)) . groupBy (==) . sort

-- part 1

gamma = fromBinary . map (snd . head . reverse . sort . countElements) . transpose -- counts.transpose 
epsilon = fromBinary . map (snd . head . sort . countElements) . transpose -- counts.transpose 

solve1 lst = gamma lst * epsilon lst

test1a = 198


-- part 2


oxygenRating = fromBinary . recFilter reverse 0 
carbonRating = fromBinary . recFilter id 0 
  --where mostCommon = map (reverse . sort . countElements) $ transpose lst

recFilter _ _ [e] = e
recFilter c n e = recFilter c (n+1) (runFilter n mostCommon e)
  where mostCommon = snd $ head $ c $ sort $ countElements $ (transpose e !! n)

runFilter n x e = filter (\s -> (s !! n) == x) e


solve2 lst = oxygenRating lst * carbonRating lst
test2a = 230

-- boiler plate

test name expected result = do
  putStrLn $ "test "++ name ++ " got "++show result
  --if result == expected then 
  --                      putStrLn $ "test "++ name ++ " success. got "++show result
  --                      else
  --                      putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


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

