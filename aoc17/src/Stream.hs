module Stream (
    solve
  , CharStream (..)
  , score
  , garbageLen
  , charStreamParser
) where

import Control.Applicative

import Text.Megaparsec (between, sepBy)
import Text.Megaparsec.Char (anyChar, char, notChar)

import Lib (Part(..), Parser, parseOrPrettyErr)


solve :: Part -> String -> String
solve pa = parseOrPrettyErr groupParser $ \str -> case pa of
  Part1 -> show . score 1 $ str
  Part2 -> show . garbageLen $ str


data CharStream = Group [CharStream] | Garbage String deriving (Show)


score :: Int -> CharStream -> Int
score _ (Garbage _     ) = 0
score s (Group   groups) = s + sum (map (score (1 + s)) groups)


garbageLen :: CharStream -> Int
garbageLen (Garbage s     ) = length s
garbageLen (Group   groups) = sum . map garbageLen $ groups


charStreamParser :: Parser CharStream
charStreamParser = groupParser <|> garbageParser


groupParser :: Parser CharStream
groupParser = Group <$> between (char '{') (char '}') (charStreamParser `sepBy` char ',')


garbageParser :: Parser CharStream
garbageParser = Garbage <$> between (char '<') (char '>') (concat <$> many garbageToks)
 where
  garbageToks :: Parser String
  garbageToks = ((char '!' >> anyChar >> return "") <|> (fmap (: "") (notChar '>')))
