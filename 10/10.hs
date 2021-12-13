import Data.Either
import Data.List

parse :: [Char] -> [Char] -> Either Char [Char] 
parse stack ('(':xs) = parse (')':stack) xs -- push
parse stack ('<':xs) = parse ('>':stack) xs -- push
parse stack ('{':xs) = parse ('}':stack) xs -- push
parse stack ('[':xs) = parse (']':stack) xs -- push
parse (s:ss) (x:xs)  -- match close brace
  | s == x = parse ss xs -- pop if we expect x
  | otherwise = Left x   -- error if unexpected
parse s [] = Right s -- return rest of stack when we run out of input

score1 chars = sum $ map score chars
  where score ')' = 3
        score ']' = 57
        score '}' = 1197
        score '>' = 25137

score2 = midpoint . sort . map (foldl (\a b -> a*5 + score b) 0)
  where score ')' = 1
        score ']' = 2
        score '}' = 3
        score '>' = 4
        midpoint arr = arr !! (length arr `div` 2)

main = do
  f <- readFile "test"
  let l = lines f
  let s1 = score1 $ lefts $ map (parse []) l
  let s2 = score2 $ rights $ map (parse []) l
  putStrLn (show s1 ++ " " ++ show s2)

