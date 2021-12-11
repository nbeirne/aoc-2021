import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Error
import Data.List

-- parse
parse = lines


bracesRound = between (char '(') (char ')')
bracesSquare = between (char '[') (char ']')
bracesCurl = between (char '{') (char '}')
bracesAngle = between (char '<') (char '>')

braces go = bracesRound go <|> bracesSquare go <|> bracesCurl go <|> bracesAngle go

line :: GenParser Char st [Char]
line = go
  where go = (many (braces go) >> return "") <|> return ""


parseLine :: String -> Either ParseError String
parseLine = Text.Parsec.Prim.parse line "(unknown)"

--parseLine :: String -> [Char]
--parseLine str = either (head . handleErr) (const "") parsed
--  where parsed = Text.Parsec.Prim.parse line "(unknown)" str


handleErr :: ParseError -> [String] -- read errors into message arr. 
handleErr = map (handle . messageString) . errorMessages
  where handle = drop 1 . reverse . drop 1

-- solve 1
solve1 = map (either (head . handleErr) (const "") . parseLine)

-- solve 2
solve2 = filter (/="") . map (solve2Line)

solve2Line str 
  | cChar /= "" = ""
  | endChar == ")" = endChar ++ (solve2Line (str ++ endChar))
  | endChar == "]" = endChar ++ (solve2Line (str ++ endChar))
  | endChar == "}" = endChar ++ (solve2Line (str ++ endChar))
  | endChar == ">" = endChar ++ (solve2Line (str ++ endChar))
  | otherwise = ""
    where parsed = parseLine str
          endChar = either (last . handleErr) (const "") parsed
          -- check if we should parse this
          cChar = either (head . handleErr) (const "") parsed


-- scores
scoreC ")" = 3
scoreC "]" = 57
scoreC "}" = 1197 
scoreC ">" = 25137
scoreC _ = 0

scoreACC ')' = 1
scoreACC ']' = 2
scoreACC '}' = 3 
scoreACC '>' = 4

scoreAC score (x:xs) = scoreAC (5*score + scoreACC x) xs
scoreAC score [] = score

score1 = sum . map scoreC
score2 lst = sorted !! idx
  where sorted = sort $ map (scoreAC 0) lst
        idx = (length sorted) `div` 2


-- test 
test1a = 26397
test2a = 288957 



-- boiler plate for running

test name expected result = do
  putStrLn $ "test "++ name ++ " got "++show result
  if result == expected then 
                        putStrLn $ "test "++ name ++ " success. got "++show result
                        else
                        putStrLn $ "test "++ name ++ " failure. got "++show result++" expected "++show expected


testAll = do
  input <- readFile "test"
  putStrLn $ "solve1: " ++ show (solve1 $ Main.parse input)
  putStrLn $ "solve2: " ++ show (solve2 $ Main.parse input)

  test "1" test1a (score1 $ solve1 $ Main.parse input)
  test "2" test2a (score2 $ solve2 $ Main.parse input)


  return ()

run = do
  input <- readFile "input"
  let a1 = score1 $ solve1 $ Main.parse input
  let a2 = score2 $ solve2 $ Main.parse input
  putStrLn $ "solve1: " ++ show a1
  putStrLn $ "solve2: " ++ show a2

main = testAll >> putStrLn "" >> run

