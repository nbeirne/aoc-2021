
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


-- cardinal points and lines

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


