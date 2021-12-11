import System.Environment (getArgs)

main = do
  input <- lines <$> (getArgs >>= readFile . head)
  let bss = map readDigits input
  let gammaBits = bits $ foldr1 addPairs $ map (1:) bss
  let epsilonBits = map (1-) gammaBits
  p 'a' $ value gammaBits * value epsilonBits
  let (oxygen, carbonDioxide) = (findRating (>=) bss, findRating (<) bss)
  p 'b' $ value oxygen * value carbonDioxide
  where
    p c result = putStrLn $ c : ": " ++ show result

readDigits :: String -> [Int]
readDigits line = map (read . (:"")) line

addPairs :: [Int] -> [Int] -> [Int]
addPairs = zipWith (+)

bits :: [Int] -> [Int]
bits (s:xs) = map comp xs
  where
    comp x | 2*x < s = 0
           | otherwise = 1

value :: [Int] -> Int
value = foldl (\v b -> 2*v+b) 0

findRating :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
findRating cmp bss = go1st $ zip bss bss
  where
    go1st [(_s, bs)] = bs
    go1st bs2s
      = go1st
      $ map    (\(_:as, bs) -> (as, bs))
      $ filter (\(a:__, bs) -> a == d) bs2s
      where
        s = length bs2s
        d = if (2*k) `cmp` s then 1 else 0
        k = sum $ map (head . fst) bs2s
