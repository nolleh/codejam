
solveNolleh :: Int -> String
solveNolleh n
  | solved == -1 = "INSOMNIA"
  | otherwise = show solved
  where solved = solve n

solve :: Int -> Int
solve n
  | n == 0 || n == -1 = -1
  | otherwise = check [0..9] (map (\i -> n*i ) [1..])

-- 모든 숫자를 제거했을때 그때의 n 을 리턴한다
check _ [] = -1
check picks (n:ns) 
  | null rest = n
  | otherwise = check rest ns
  where rest = check_for_digits picks (digits n)

-- 더 체크해야하는 리스트를 리턴한다
check_for_digits picks [] = picks
check_for_digits picks (d:ds) = check_for_digits rest ds
  where rest = filter (/=d) picks

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
 
printOutput :: (Int, String) -> IO ()
printOutput (i, a) = do
  putStr $ "Case #" ++ show i ++ ": "
  putStrLn $ a
 
main = do
  c <- getContents
  let (l:ls) = lines c
  let r = map solveNolleh (map read ls)
  mapM_ printOutput (zip [1..] r)

 