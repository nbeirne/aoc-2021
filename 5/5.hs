import qualified Data.Map as Map
import Data.Map (Map)

import Data.List

type Point = (Int,Int)
type Line = (Point,Point)

vector a b = [a,a+(signum (b-a))..b]

straightPointsInLine :: Line -> [Point]
straightPointsInLine ((x,y),(x',y')) = if (x == x' || y == y') then zip xp yp else []
  where xs = vector x x'
        ys = vector y y'
        xp = if drop 1 xs == [] then xs else xs
        yp = if drop 1 ys == [] then ys else ys



pointsInLine :: Line -> [Point]
pointsInLine ((x,y),(x',y')) = zip xp yp 
  where xs = vector x x'
        ys = vector y y'
        xp = if drop 1 xs == [] then xs else xs
        yp = if drop 1 ys == [] then ys else ys


insertPoint :: Point -> Map Point Int -> Map Point Int 
insertPoint point = Map.insertWith (\a b -> a+b) point 1


countPoints :: [Point] -> Map Point Int
countPoints = foldr insertPoint Map.empty

-- parsing
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'

parse :: String -> [Line]
parse = map parseLine . lines



parseLine :: String -> Line
parseLine s = (parsePoint a, parsePoint b)
  where w = words s
        a = w !! 0
        b = w !! 2
        parsePoint p = (read a, read b)
          where [a,b] = splitBy (==',') p


-- solutions
solve1 ls = countPoints $ ps
  where ps = ls >>= straightPointsInLine


solve2 ls = countPoints $ ps
  where ps = ls >>= pointsInLine

-- scoring a Map based on overlaps
score = length . Map.filter (>=2) 

test1a = 5
test2a = 12

-- boiler plate

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn ""
  putStrLn $ "solve2: " ++ show (solve2 $ parse input)
  putStrLn ""

  test "1" test1a (score $ solve1 $ parse input)
  test "2" test2a (score $ solve2 $ parse input)
  return ()

solveAll = do
  input <- readFile "input"
  let a1 = score $ solve1 $ parse input
  putStrLn $ "solve1: " ++ show a1
  let a2 = score $ solve2 $ parse input
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

