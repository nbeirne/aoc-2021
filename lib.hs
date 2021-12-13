import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'




-- run function on the first input, then the first two, then first 3, etc. Call with the first arg as an empty list.
iter :: [a] -> [a] -> ([a] -> b) -> [b]
iter _ [] _ = []
iter x (y:ys) f = f x':iter x' ys f
  where x' = x++[y]


-- change idx in array with f. unsafe and innefficient.
change :: Int -> (a -> a) -> [a] -> [a]
change i f xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs


-- cardinal points and lines

type Board = [[Int]]
type Point = (Int,Int)
type Line = (Point,Point)

-- find numbers from a-b where b may be less than a. This may have an infinite list when a=b.
vector a b = [a,a+(signum (b-a))..b]

-- horizontal and vertical points lines
straightPointsInLine :: Line -> [Point]
straightPointsInLine ((x,y),(x',y')) = if (x == x' || y == y') then zip xp yp else []
  where xs = vector x x'
        ys = vector y y'
        xp = if drop 1 xs == [] then xs else xs
        yp = if drop 1 ys == [] then ys else ys


-- horizontal, vertical, and 45 degree diagonal lines 
pointsInLine :: Line -> [Point]
pointsInLine ((x,y),(x',y')) = zip xp yp 
  where xs = vector x x'
        ys = vector y y'
        xp = if drop 1 xs == [] then xs else xs
        yp = if drop 1 ys == [] then ys else ys

parse :: String -> Board
parse = map (map (read . (:[]))) . lines

adj :: Point -> Set Point
adj (x,y) = Set.fromList $ [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1], x /= x' || y /= y']

val :: Board -> Point -> Int
val b (x,y) = (b !! x) !! y

toStr :: Set Point -> String
toStr s = unlines [makeLine y | y <- [0..maxY]]
  where maxX = maximum $ Set.toList $ Set.map fst s
        maxY = maximum $ Set.toList $ Set.map snd s
        makeLine y = [c | x <- [0..maxX], let c = if Set.member (x,y) s then '#' else ' ']
