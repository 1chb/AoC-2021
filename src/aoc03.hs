import System.Environment (getArgs)

main = do
  input <- lines <$> (getArgs >>= readFile . head)
  let bss = map readDigits input
  let gammaBits = bits (length bss) $ foldr1 sums bss
  let epsilonBits = map (1-) gammaBits
  p 'a' $ value gammaBits * value epsilonBits
  p 'b' $ value (find (>=) bss) * value (find (<) bss)
  where
    p c result = putStrLn $ c : ": " ++ show result

readDigits :: String -> [Int]
readDigits line = map (read . (:"")) line

sums :: [Int] -> [Int] -> [Int]
sums = zipWith (+)

bits :: Int -> [Int] -> [Int]
bits s xs = map comp xs
  where
    comp x | 2*x < s = 0
           | otherwise = 1

value :: [Int] -> Int
value = foldl (\v b -> 2*v+b) 0

find :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
find cmp bss = go1st $ zip bss bss
  where
    go1st [(_s, bs)] = bs
    go1st bs2s
      = go1st
      $ map (\(_:as, bs) -> (as, bs))
      $ filter (\(a:_, bs) -> a == d) bs2s
      where
        s = length bs2s
        d = if (2*k) `cmp` s then 1 else 0
        k = sum $ map (head . fst) bs2s
        
