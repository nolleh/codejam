import Data.List
import Data.Maybe

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a
fact_jam n j = take j
  $ map (\sq -> ((second sq), (third sq)))
  $ filter (\sq -> (first sq) == True)
  $ map ck_jam 
  $ seq_jam n

seq_jam :: Int -> [[Int]]
seq_jam n = map (\xs-> [1]++xs++[1]) (seq_jam_nest (n-2))

seq_jam_nest :: Int -> [[Int]]
seq_jam_nest n = sequence $ replicate n [0,1]

ck_jam :: [Int] -> (Bool, Int, [Int])
ck_jam d 
  | not.any (isPrime) $ numbers = 
      (True, (decimal 10 d), map (\n -> (divisor n [2..])) numbers)
  | otherwise = (False, 0, [0])
  where numbers = decimals [2..10] d

divisor :: Int -> [Int] -> Int
divisor _ [] = 0
divisor n (d:ds)
  | mod n d == 0 = d
  | otherwise = divisor n ds

decimals [] _ = []
decimals (d:ds) list = (decimal d list):(decimals ds list)

decimal :: Int -> [Int] -> Int
decimal d = sum . (unf d)
unf _ [] = []
unf decimal (x:xs) = (x * decimal ^ (length xs)):(unf decimal xs)

main = do
  c <- getContents
  let (l:ls) = lines c
  putStrLn $ "Case #1:"
  let (r:_) = map parse ls
  mapM_ printOutput (fact_jam (head r) (head (tail r)))

parse :: String -> [Int]
parse l = map read (words l)

printOutput :: (Int, [Int]) -> IO ()
printOutput r = do
  putStrLn $ show (fst r) ++ " " ++ printList (snd r)

printList l =
  concatMap (\a -> show a ++ " ") l

pfactors prs n = unfoldr (\(ds,n) -> listToMaybe  
  [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++ [n|n>1], mod n x==0]) (prs,n)
primes :: [Int]
primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]

isPrime n = 
  n > 1 && foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes