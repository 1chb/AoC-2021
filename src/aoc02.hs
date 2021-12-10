{-# LANGUAGE LambdaCase #-}
import Control.Applicative ((<|>))
import Data.Char (isDigit, isSpace)
import System.Environment (getArgs)
import qualified Text.ParserCombinators.ReadP as P

main = do
  input <- parse cmds <$> (getArgs >>= readFile . head)
  -- print input
  p 'a' $ uncurry (*) $ foldl step (0,0) input
  p 'b' $ uncurry (*) $ fst $ foldl step2 ((0,0),0) input
  where
    p c result = putStrLn $ c : ": " ++ show result

parse :: P.ReadP a -> String -> a
parse p = head . fmap fst . P.readP_to_S (p <* P.skipSpaces <* P.eof)

data Cmd = Forward Int | Down Int | Up Int deriving (Show)

step :: (Int, Int) -> Cmd -> (Int, Int)
step (x, d) = \case
  Forward dx -> (x+dx, d)
  Down    dd -> (x, d+dd)
  Up      dd -> (x, d-dd)

step2 :: ((Int, Int), Int) -> Cmd -> ((Int, Int), Int)
step2 ((x, d), aim) = \case
  Forward dx -> ((x+dx, d+dx*aim), aim)
  Down    dd -> ((x, d), aim+dd)
  Up      dd -> ((x, d), aim-dd)

cmds :: P.ReadP [Cmd]
cmds = P.sepBy1 cmd space

cmd :: P.ReadP Cmd
cmd = ($) <$> (forward <|> down <|> up) <* space <*> number

forward :: P.ReadP (Int -> Cmd)
forward = P.string "forward" >> return Forward

down :: P.ReadP (Int -> Cmd)
down = P.string "down" >> return Down

up :: P.ReadP (Int -> Cmd)
up = P.string "up" >> return Up

number :: P.ReadP Int
number = read <$> P.many1 (P.satisfy isDigit)

space :: P.ReadP String
space = P.munch1 isSpace
