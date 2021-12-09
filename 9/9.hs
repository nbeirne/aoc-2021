import Control.Lens
import Data.Maybe

parse :: String -> [[Int]]
parse = map (map read . map (:[])) . lines

type Point = (Int,Int)
type Board = [[Int]]

m :: Board
m = [[1,2,3],
     [4,5,6],
     [7,8,9]]

val :: Board -> Point -> Int
val b (x,y) = (b !! x) !! y

valS :: Board -> Point -> Maybe Int
valS b (x,y) = (b ^? element x) >>= (\b' -> b' ^? element y)

adj :: Board -> Point -> [Int]
adj bd (x,y) = [a,b,c,d] >>= id
  where a = maybe [] (:[]) $ valS bd (x-1,y)
        b = maybe [] (:[]) $ valS bd (x+1,y)
        c = maybe [] (:[]) $ valS bd (x,y-1)
        d = maybe [] (:[]) $ valS bd (x,y+1)

isLowPoint b p = all (\pt -> pt > v) a
  where v = val b p
        a = adj b p


findLowPoints :: Board -> [Point]
findLowPoints b = pts
  where pts = [(x,y) | x <- [0..length b - 1], y <- [0..length (b !! x) - 1], isLowPoint b (x,y)]


solve1 b = map (val b) $ findLowPoints b
solve2 = id

-- score a solution based on the id
score1 = sum . map (+1)
score2 = const 0

test1a = 15
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
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn $ "solve2: " ++ show (solve2 $ parse input)

  test "1" test1a (score1 $ solve1 $ parse input)
  test "2" test2a (score2 $ solve2 $ parse input)


  return ()

solveAll = do
  input <- readFile "input"
  let a1 = score1 $ solve1 $ parse input
  let a2 = score2 $ solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

