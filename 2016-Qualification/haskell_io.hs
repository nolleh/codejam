groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)
 
parseNolleh :: [String] -> (Int, Int, [Int])
parseNolleh [a, b, c] = (read a, read b, map read (words c))
 
solveNolleh _ = (3, 10)
 
printOutput (i, (a,b)) = do
  putStr $ "Case #" ++ show i ++ ":"
  putStrLn $ show a ++ " " ++ show b
 
main = do
  c <- getContents
  let (l:ls) = lines c
  let r = map solveNolleh $ map parseNolleh $ groupN 3 ls
  mapM_ printOutput (zip [1..] r)


-- 1 
-- 100
-- 3
-- 5 75 25
 