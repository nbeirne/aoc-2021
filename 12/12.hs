import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

type M = Map String (Set String)

-- parse
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'


parseLine :: String -> M -> M
parseLine s m = ins a b $ ins b a m
  where (a:b:[]) = splitBy (=='-') s
        ins k = Map.insertWith Set.union k . Set.singleton

parse = foldr parseLine Map.empty . lines


-- run solution
findPaths :: ([String] -> String -> Bool) -> M -> [String] -> [[String]]
findPaths _ _ path@("end":_) = [path] -- reached end
findPaths isPathValid mp path@(cur:_)
  | vp == [] = [] -- no paths found
  | otherwise = map (\p -> findPaths isPathValid mp (p:path)) vp >>= id
  where vp = filter (isPathValid path) $ Set.toList $ mp Map.! cur -- find points you can travel to
  

isPathValid1 path dest = (all isUpper dest) || (not $ dest `elem` path)

isPathValid2 path "start" = False
isPathValid2 path dest = (all isUpper dest) || noDups || (not $ dest `elem` path)
  where lw = filter (all isLower) path
        noDups = (Set.size $ Set.fromList lw) == length lw -- you can visit if there are 0 lower dups

-- solve
solve1 mp = findPaths isPathValid1 mp ["start"]
solve2 mp = findPaths isPathValid2 mp ["start"]

score = length

-- test 
test1a = 10
test1b = 19
test1c = 226

test2a = 36
test2b = 103
test2c = 3509

-- boiler plate for running

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testF f exp1 exp2 = do
  input <- readFile f
  test (f++" 1") exp1 (score $ solve1 $ parse input)
  test (f++" 2") exp2 (score $ solve2 $ parse input)
  return ()

testAll = do
  testF "test1" test1a test2a
  testF "test2" test1b test2b
  testF "test3" test1c test2c

run = do
  input <- readFile "input"
  let a1 = score $ solve1 $ parse input
  let a2 = score $ solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> run

