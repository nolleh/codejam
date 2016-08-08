cake_flip = flips . reverse 
flips [] = 0
flips stack@(x:xs) 
  | x == '-' = 1 + flips (flip_stack xs)
  | otherwise = flips xs

flip_stack:: String -> String
flip_stack list = map flip_one list

flip_one:: Char -> Char
flip_one pan
  | pan == '-' = '+'
  | otherwise = '-'

main = do
  c <- getContents
  let (l:ls) = lines c
  let r = map cake_flip ls
  mapM_ printOutput (zip [1..] r)


printOutput (i, a) = do
  putStr $ "Case #" ++ show i ++ ": "
  putStrLn $ show a