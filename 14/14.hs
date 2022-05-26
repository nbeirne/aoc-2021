import qualified Data.Map as Map
import Data.Map (Map, (!))
import Data.List


tm = Map.fromList [
                   
                   ("CH",'B'),
                   ("HH",'N'),
                   ("CB",'H'),
                   ("NH",'C'),
                   ("HB",'C'),
                   ("HC",'B'),
                   ("HN",'C'),
                   ("NN",'C'),
                   ("BH",'H'),
                   ("NC",'B'),
                   ("NB",'B'),
                   ("BN",'B'),
                   ("BB",'N'),
                   ("BC",'B'),
                   ("CC",'N'),
                   ("CN",'C')
                  ]

ti :: Map String Int
ti = Map.fromList [("NN",1),
                   ("NC",1),
                   ("CB",1)
                  ]

-- parse

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy f s = case dropWhile f s of 
                [] -> []
                s' -> w:splitBy f s''
                      where (w, s'') = break f s'


parse s = (ls !! 0 !! 0, Map.fromList $ map parseIns mp)
  where ls = splitBy (=="") $ lines s
        mp = (ls !! 1)

parseIns i = (ws !! 0, ws !! 2 !! 0)
  where ws = words i

-- solving 

step :: Map String Char -> String -> String
step mp str = (foldl (process mp) [] $ zip str (drop 1 str)) ++ [last str]
  where process mp s (a,b) = a:mp ! [a,b]:s


step' :: Map String Char -> Map String Int -> Map String Int -- track just the pairs since order does not matter. 
step' mp pairs = Map.foldrWithKey (\k v a -> Map.unionWith (+) a $ Map.fromList $ map (flip (,) v) $ adjPairs $ newSubstr mp k) Map.empty pairs

newSubstr :: Map String Char -> String -> String
newSubstr mp str = intersperse (mp ! str) str

adjPairs :: String -> [String]
adjPairs str = map (\(a,b) -> a:b:[]) $ zip str (drop 1 str) 

--mk = Map. . map (flip (,) 1) . adjPairs 
mk str = foldr (\k a -> Map.insertWith (+) k 1 a) Map.empty (adjPairs str)

steps mp str = iterate (step' mp) (mk str)


solve1 :: (String, Map String Char) -> Map String Int
solve1 (str,mp) = steps' mp str !! 10

solve2 :: (String, Map String Char) -> Map String Int
solve2 (str,mp) = steps' mp str !! 40


-- smaller solve
steps' mp str = iterate (Map.foldrWithKey (\k v a -> Map.unionWith (+) a $ Map.fromList $ map (flip (,) v) $ map (\(a,b) -> a:b:[]) $ (zip <$> id <*> drop 1) $ newSubstr mp k) Map.empty) . foldr (flip (Map.insertWith (+)) 1) Map.empty . map (\(a,b) -> a:b:[]) $ zip str (drop 1 str) 
 

-- scores

insString :: String -> Map Char Int
insString = foldr (\x m -> Map.insertWith (+) x 1 m) Map.empty

charMap :: Map String Int -> Map Char Int
charMap mp = Map.foldrWithKey (\k v acc -> Map.unionWith (+) acc $ Map.map (*v) $ insString k) Map.empty mp

score1 :: Map String Int -> Int
score1 mp = (mx - lx) `div` 2
  where s = charMap mp
        (_,mx) = Map.foldrWithKey (\c n (c',n') -> if n'>n then (c',n') else (c,n)) (Map.elemAt 0 s) s
        (_,lx) = Map.foldrWithKey (\c n (c',n') -> if n'<n then (c',n') else (c,n)) (Map.elemAt 0 s) s

score2 :: Map String Int -> Int
score2 = score1

-- test 
test1a = 1588
test2a = 2188189693529


-- boiler plate for running

test name expected result = do
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "solve1: " ++ show (solve1 $ parse input)
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

-- 2967977072189 is too high
