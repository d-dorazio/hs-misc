module PermutationPromenade (
    solve
  , Pos
  , DanceMove (..)
  , interpretDanceMoves
  , danceMoveParser
) where

import Control.Applicative ((*>), (<|>))

import Data.Foldable (toList)
import Data.List (foldl')

import qualified Data.Sequence as Seq

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, letterChar)

import Lib (Part(..), Parser, parseOrPrettyErr, decimal, symbol)


solve :: Part -> String -> String
solve part = parseOrPrettyErr danceMovesParser $ \moves ->
  let ini = interpretDanceMoves ['a' .. 'p'] moves in go moves (ini, Seq.singleton ['a' .. 'p'])
 where
  danceMovesParser = danceMoveParser `sepBy` symbol ","

  -- the idea is that eventually the sequence repeats, therefore we keep an array with
  -- what we've seen so far and as soon as we find a configuration we already found then
  -- we already have all the different configurations in the system and it's just a matter
  -- of figuring out what's the configuration at the given position
  go moves (progs, seen) = case Seq.elemIndexL progs seen of
    Just _  -> let cycleSize = Seq.length seen in Seq.index seen (iterations `mod` cycleSize)
    Nothing -> go moves (interpretDanceMoves progs moves, seen Seq.|> progs)

  iterations :: Int
  iterations = case part of
    Part1 -> 0
    Part2 -> 1000000000


type Pos = Int
data DanceMove = Spin Int | Exchange Pos Pos | Partner Char Char deriving (Show)

interpretDanceMoves :: String -> [DanceMove] -> String
interpretDanceMoves iniProgs = toList . foldl' interpret (Seq.fromList iniProgs)
 where
  interpret progs (Spin c) = let (l, r) = Seq.splitAt (Seq.length progs - c) progs in r Seq.>< l
  interpret progs (Exchange i j) =
    let ci = Seq.index progs i
        cj = Seq.index progs j
    in  Seq.update i cj . Seq.update j ci $ progs
  interpret progs (Partner ci cj) =
    let (Just i) = Seq.elemIndexL ci progs
        (Just j) = Seq.elemIndexL cj progs
    in  interpret progs (Exchange i j)


danceMoveParser :: Parser DanceMove
danceMoveParser = spinP <|> exchangeP <|> partnerP
 where
  spinP     = Spin <$> (char 's' *> decimal)
  exchangeP = do
    _ <- char 'x'
    f <- decimal
    _ <- char '/'
    s <- decimal
    return $ Exchange f s
  partnerP = do
    _ <- char 'p'
    f <- letterChar
    _ <- char '/'
    s <- letterChar
    return $ Partner f s
