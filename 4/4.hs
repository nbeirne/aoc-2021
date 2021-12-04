import Data.List

-- parsing
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'

parseBoards :: [String] -> [[[Int]]]
parseBoards l = map parse b
  where b = splitBy (=="") l
        parse = map (map read . words)


parse s = (draws,boards)
  where l = lines s
        draws = map read $ splitBy (==',') (l !! 0) :: [Int]
        boards = parseBoards (drop 2 l)

-- helpers

isWin :: [Int] -> [[Int]] -> Bool
isWin draws board = rowsWin conditions
  where board' = transpose board
        rowsWin = any (==True) . map rowWin
        rowWin = all (flip elem draws)
        conditions = board ++ board'

score draws board = sum unpicked
  where unpicked = filter (not . flip elem draws) (board >>= id) {- flatten -}

-- run function on the first input, then the first two, then first 3, etc. Call with the first arg as an empty list.
iter :: [a] -> [a] -> ([a] -> b) -> [b]
iter _ [] _ = []
iter x (y:ys) f = f x':iter x' ys f
  where x' = x++[y]

-- solution 1

-- find first winner
findFirstWinner :: [Int] -> [[[Int]]] -> Maybe ([Int],[[Int]])
findFirstWinner draws boards = winner
  where f d = map (\m -> (d,m)) $ filter (isWin d) boards
        winner = find ((/=) []) (iter [] draws f) >>= return . head


solve1 (d,b) = (score draws winner) * (last draws)
  where (Just (draws,winner)) = findFirstWinner d b


-- last winner
findLastWinner :: Int -> [Int] -> [[[Int]]] -> ([Int],[[Int]])
findLastWinner n draws [board] 
  | isWin (take n draws) board = (take n draws, board)
  | otherwise = findLastWinner (n+1) draws [board]
findLastWinner n draws boards = findLastWinner (n+1) draws remainingWinners
  where findWinners d = map (\m -> (d,m)) . filter (isWin d) 
        drawAtN = take n draws
        winners = findWinners drawAtN boards
        -- remove all winners at current iteration. if there are winning thing
        --boards' = filter (not $ elem (map snd $ winners)) boards
        remainingWinners = filter (not . flip elem (map snd winners)) $ boards


--solve2 (d,b) = (draws,winner)
solve2 (d,b) = (score draws winner) * (last draws)
  where (draws,winner) = findLastWinner 0 d winnersOnly
        findWinners d = map (\m -> (d,m)) . filter (isWin d) 
        winnersOnly = map snd $ findWinners d b

test1a = 4512
test2a = 1924

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


test2 = do 
  input <- readFile "test"
  let s = solve2 $ parse input
  putStrLn $ show $ s


-- 8505 is too low.
