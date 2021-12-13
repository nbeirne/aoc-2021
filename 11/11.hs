import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

type Point = (Int,Int)
type Board = Map Point Int


t1 = parse "1111119991191911999111111"

-- parse
parse :: String -> Board
parse = Map.fromList . map (\(i,v) -> ((i `div` 10, i `mod` 10),v)) . zip [0..] . map (read . (:[])) . concat . lines


-- helpers
adj (x,y) = Set.fromList $ [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x /= x' || y /= y']
val b p = b Map.! p




-- reset board
resetBoard = Map.map (\v -> if v > 9 then 0 else v)

-- flash entire board
inc = Map.map (+1)

-- flash p and increment adjasent points
flashPt pt board = Set.foldr (Map.adjust (+1)) board (adj pt)

-- find flashes in board after increment
flashes = Set.fromList . Map.keys . Map.filter (>9) 

-- run flashes for a step
flashesIter :: Board -> Set Point -> (Board,Set Point)
flashesIter board flashed 
  | Set.null newPts = (board,flashed) -- base case. when we have no more flashes. 
  | otherwise = flashesIter b' (Set.union flashPts newPts) -- recursive call
  where flashPts = flashes board 
        newPts = flashPts `Set.difference` flashed -- get points we have not seen yet
        b' = Set.foldr flashPt board newPts -- flash the new points

  
-- inc, run flashes, reset. 
step :: (Board,Int) -> (Board, Int)
step (b,_) = (resetBoard b', flashSize)
  where (b',flashed) = flashesIter (inc b) Set.empty
        flashSize = Set.size flashed

-- solve 1
solve1 b = iterate step (b,0)
solve2 = solve1


-- scores
score1 = sum . take 101 . map snd
score2 = fst . head . filter (\(_,(_,c)) -> c==100) . zip [0..] -- take first where all flash

-- test 



-- boiler plate 
test1a = 1656 
test2a = 195

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

