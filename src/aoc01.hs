import Data.Char (isDigit, isSpace)
import System.Environment (getArgs)
import qualified Text.ParserCombinators.ReadP as P

main = do
  input <- parse numbers1 <$> (getArgs >>= readFile . head)
  p 'a' $ nInc input
  p 'b' $ nInc $ zipWith3 add3 input (tail input) (tail $ tail input)
  where
    p c result = putStrLn $ c : ": " ++ show result
    nInc input = length $ filter (uncurry (<)) $ zip input $ tail input
    add3 a b c = a+b+c

parse :: P.ReadP a -> String -> a
parse p = head . fmap fst . P.readP_to_S (p <* P.skipSpaces <* P.eof)

numbers1 :: P.ReadP [Int]
numbers1 = P.sepBy1 number space

number :: P.ReadP Int
number = read <$> P.many1 (P.satisfy isDigit)

space :: P.ReadP String
space = P.munch1 isSpace
