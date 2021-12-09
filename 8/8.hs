import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'

type SegMap = Map Char (Set Char)

parse = map parseLine . lines
parseLine input = (map Set.fromList (words is),map Set.fromList (words os))
  where (is:os:[]) = splitBy (=='|') input


-- there is a 4-digit 7-segment displays
-- each digit is rendered by turning on or off any of the segments (a-g)
--
-- 1: a,c
-- 7: a,c,f
-- 4: b,c,d,f
-- 2: a,c,d,e,g
-- 3: a,c,d,f,g
-- 5: a,b,d,f,g
-- 0: a,b,c,e,f,g
-- 6: a,b,d,e,f,g
-- 9: a,b,c,d,f,g
-- 8: a,b,c,d,e,f,g
--
-- signals mixed up. wires are connected randomly. you might know b and g are on, but not segments b and g. only a digit that uses 2 segments.
--
-- there are 10 unique signal inputs to each problem. they produce an output value.


solve1 = sum . map solveLine 
    where solveLine (_,cases) = length $ filter (flip elem [2,3,4,7]) $ map length cases


-- 2nd part

-- build inputs into an dictionary from numbers -> values
reduceInputs :: [Set Char] -> Map (Set Char) Int
reduceInputs inputs = Map.fromList [(n0,0),
                                    (n1,1),
                                    (n2,2),
                                    (n3,3),
                                    (n4,4),
                                    (n5,5),
                                    (n6,6),
                                    (n7,7),
                                    (n8,8),
                                    (n9,9)]
  where n1 = (filter ((==2) . length ) inputs) !! 0
        n4 = (filter ((==4) . length ) inputs) !! 0
        n7 = (filter ((==3) . length ) inputs) !! 0
        n8 = (filter ((==7) . length ) inputs) !! 0
        a = Set.difference n7 n1
        eg = invertedSet (Set.union n4 n7) -- segments e and g
        n2 = (filter (\l -> (length l == 5) && eg `Set.isProperSubsetOf` l) inputs) !! 0
        n9 = (filter (\l -> (length l == 6) && not (eg `Set.isProperSubsetOf` l)) inputs) !! 0
        n0 = Set.unions [n7, invertedSet n2, invertedSet n4]
        n6 = (filter (\l -> (length l == 6) && l /= n0 && l /= n9) inputs) !! 0
        d = invertedSet n0
        g = invertedSet $ Set.unions [n4, n7, invertedSet n9]
        n3 = Set.unions [n1, a, d, g]
        n5 = Set.unions [invertedSet n2, a, d, g]

solveLine :: ([Set Char], [Set Char]) -> Int
solveLine (inputs,cases) = (a*1000) + (b*100) + (c*10) + d
  where m = reduceInputs inputs
        a = m Map.! (cases !! 0)
        b = m Map.! (cases !! 1)
        c = m Map.! (cases !! 2)
        d = m Map.! (cases !! 3)


invertedSet :: Set Char -> Set Char
invertedSet = (Set.\\) full 
  where full = (Set.fromList "abcdefg") 
  
solve2 = sum . map solveLine

-- score a solution based on the id
score = id

line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
test1a = 26
test2a = 61229

-- boiler plate

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
  putStrLn $ "solve2: " ++ show (solve2 $ parse input)

  --putStrLn $ "runIter: " ++ (show $ reduceCases $ map Set.fromList ["bdg", "cedg"])

  --test "1-1" test11a (score $ solve1 $ parse "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
  test "1" test1a (score $ solve1 $ parse input)
  test "2" test2a (score $ solve2 $ parse input)


  return ()

solveAll = do
  input <- readFile "input"
  let a1 = solve1 $ parse input
  let a2 = score $ solve2 $ parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> solveAll

