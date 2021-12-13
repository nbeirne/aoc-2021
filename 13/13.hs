import qualified Data.Set as Set
import Data.Set (Set)

type Point = (Int,Int)
type Paper = Set Point

data Ins = Vert Int | Horiz Int deriving (Show)

-- parse

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'

parse :: String -> (Paper, [Ins])
parse string = (p, ins)
  where l = splitBy (=="") $ lines string
        p = Set.fromList $ map parsePoint $ l !! 0
        ins = map parseIns $ l !! 1

parsePoint :: String -> (Int,Int)
parsePoint = arrToTuple . map read . splitBy (==',')
  where arrToTuple (x:y:[]) = (x,y)

parseIns :: String -> Ins
parseIns l = parse (i !! 2)
  where i = words l
        parse ('x':_:xs) = Horiz (read xs)
        parse ('y':_:xs) = Vert (read xs)


--fold :: Paper -> Ins -> Paper
fold p (Horiz sx) = Set.union (Set.map flip fp) sp
  where (fp,sp) = Set.partition ((>sx) . fst) p 
        flip (x,y) = ((2*sx)-x,y)
fold p (Vert sy) = Set.union (Set.map flip fp) sp
  where (fp,sp) = Set.partition ((>sy) . snd) p 
        flip (x,y) = (x,(2*sy)-y)

-- solve 1
solve1 (p,i:_) = fold p i


-- solve 2

toStr :: Set Point -> String
toStr s = unlines [makeLine y | y <- [0..maxY]]
  where maxX = maximum $ Set.toList $ Set.map fst s
        maxY = maximum $ Set.toList $ Set.map snd s
        makeLine y = [c | x <- [0..maxX], let c = if Set.member (x,y) s then '#' else ' ']

solve2 (p,ins) = toStr $ foldl fold p ins


-- scores
score1 = Set.size
score2 = const 0


-- test 
test1a = 17
test2a = 0



-- boiler plate for running

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn $ "solve2: \n" ++ (solve2 $ parse input)
  test "1" test1a (score1 $ solve1 $ parse input)
  test "2" test2a (score2 $ solve2 $ parse input)
  return ()

run = do
  input <- readFile "input"
  let a1 = score1 $ solve1 $ parse input
  let a2 = solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: \n" ++ a2

main = testAll >> putStrLn "" >> run

