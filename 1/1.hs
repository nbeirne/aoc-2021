import System.IO

run1 :: [Int] -> Int
run1 (x:y:xs) = (if x < y then 1 else 0)+ run1 (y:xs)
run1 _ = 0

run2 :: [Int] -> Int
run2 (a:b:c:d:xs) = (if (a+b+c) < (b+c+d) then 1 else 0)+ run2 (b:c:d:xs)
run2 _ = 0



problem1 = readFile "input" >>= (return . show . run1 . map read . lines) >>= putStrLn 
problem2 = readFile "input" >>= (return . show . run2 . map read . lines) >>= putStrLn 
main = problem1 >> problem2
