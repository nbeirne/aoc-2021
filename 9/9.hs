import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List

import qualified Data.Set as Set
import Data.Set (Set)

parse :: String -> [[Int]]
parse = map (map read . map (:[])) . lines

type Point = (Int,Int)
type Board = [[Int]]

m :: Board
m = [[1,2,1],
     [4,5,6],
     [7,8,9]]

val :: Board -> Point -> Int
val b (x,y) = (b !! x) !! y

valSafe :: Board -> Point -> Maybe Int
valSafe b (x,y) = (b ^? element x) >>= (\b' -> b' ^? element y)

adjPts (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

adjVals :: Board -> Point -> [(Point,Int)]
--adjVals bd pt = map (maybe [] (:[]) . valSafe bd) (adjPts pt) >>= id
adjVals bd pt = do
  pt' <- adjPts pt
  guard $ not $ Nothing == valSafe bd pt'
  return (pt', val bd pt')

isLowPoint b p = all (\pt -> pt > v) a
  where v = val b p
        (_,a) = unzip $ adjVals b p


findLowPoints :: Board -> [Point]
findLowPoints b = pts
  where pts = [(x,y) | x <- [0..length b - 1], y <- [0..length (b !! x) - 1], isLowPoint b (x,y)]


solve1 b = map (val b) $ findLowPoints b

findBasinR b (set, basin) 
  | null set = basin
  | otherwise = findBasinR b (Set.union unseenPts ps, b')
  where p = Set.elemAt 0 set -- get point
        ps = Set.deleteAt 0 set -- remove that point
        adjs = adjVals b p -- get adjasent
        (includePts,_) = unzip $ filter ((/=) 9 . snd) adjs -- filter out points which are lt pointval
        unseenPts = Set.difference (Set.fromList includePts) basin
        b' = if includePts /= [] then Set.insert p basin else basin -- add p if this is not the top point


findBasin :: Board -> Point -> Set Point
findBasin b lowPoint = findBasinR b (Set.singleton lowPoint, Set.empty)

solve2 b = map (findBasin b) lowPoints
  where lowPoints = findLowPoints b
        
  

-- score a solution based on the id
score1 = sum . map (+1)
score2 = product . take 3 . reverse . sort . map Set.size
--score2 = Set

test1a = 15
--test2a = 14
test2a = 1134

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

