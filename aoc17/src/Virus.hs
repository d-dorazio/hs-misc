module Virus (
    solve
  , Cell (..)
  , Pos
  , Direction
  , rotateDirectionCw
  , rotateDirectionCCw
  , invertDirection
  , dirToPos
  , posAdd
  , part1Infect
  , part2Infect
  , virusBurst
  , cellsParser
  , cellParser
) where

import Control.Applicative ((<|>), some)
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)

import Data.Maybe (fromMaybe)

import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Basic as HTB

import Text.Megaparsec (getPosition, sourceLine, sourceColumn, unPos)

import Lib (Part(..), Parser, parseOrPrettyErr, symbol)


solve :: Part -> String -> String
solve part = parseOrPrettyErr
  cellsParser
  (show . length . filter (== Infected) . virusBurst infect iterations)
 where
  (infect, iterations) = case part of
    Part1 -> (part1Infect, 10000)
    Part2 -> (part2Infect, 10000000)


data Cell = Clean | Infected | Weakened | Flagged deriving (Eq, Show)
type Pos = (Int, Int)
data Direction = U | R | D | L deriving (Show)

rotateDirectionCw :: Direction -> Direction
rotateDirectionCw U = R
rotateDirectionCw R = D
rotateDirectionCw D = L
rotateDirectionCw L = U

rotateDirectionCCw :: Direction -> Direction
rotateDirectionCCw = invertDirection . rotateDirectionCw

invertDirection :: Direction -> Direction
invertDirection U = D
invertDirection R = L
invertDirection D = U
invertDirection L = R

dirToPos :: Direction -> Pos
dirToPos U = (-1, 0)
dirToPos R = (0, 1)
dirToPos D = (1, 0)
dirToPos L = (0, -1)

posAdd :: Pos -> Pos -> Pos
posAdd (x, y) (x', y') = (x + x', y + y')

part1Infect :: Direction -> Cell -> (Direction, Cell)
part1Infect dir Clean    = (rotateDirectionCCw dir, Infected)
part1Infect dir Infected = (rotateDirectionCw dir, Clean)
part1Infect _   cell     = error ("part1 does not work with cell " ++ show cell)

part2Infect :: Direction -> Cell -> (Direction, Cell)
part2Infect dir Clean    = (rotateDirectionCCw dir, Weakened)
part2Infect dir Weakened = (dir, Infected)
part2Infect dir Infected = (rotateDirectionCw dir, Flagged)
part2Infect dir Flagged  = (invertDirection dir, Clean)

virusBurst :: (Direction -> Cell -> (Direction, Cell)) -> Int -> [(Pos, Cell)] -> [Cell]
virusBurst infect n cells = runST $ do
  cellsMap <- HT.fromList cells :: ST a (HTB.HashTable a Pos Cell)

  let burst ((pos, dir), hist) _ = do
        mcurrentCell <- HT.lookup cellsMap pos
        let currentCell          = fromMaybe Clean mcurrentCell
        let (dir', currentCell') = infect dir currentCell
        let pos'                 = posAdd pos . dirToPos $ dir'

        HT.insert cellsMap pos currentCell'
        return ((pos', dir'), currentCell' : hist)

  let state = (((centre, centre), U), [])
  (reverse . snd) <$> foldM burst state [1 .. n]
 where
  centre :: Int
  centre = ceiling (sqrt (fromIntegral $ length cells) :: Float) `div` 2

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
cellsParser :: Parser [(Pos, Cell)]
cellsParser = some itemParser
 where
  itemParser = do
    pos <- getPosition
    dir <- cellParser
    let row = unPos (sourceLine pos) - 1
    let col = unPos (sourceColumn pos) - 1
    return ((row, col), dir)

cellParser :: Parser Cell
cellParser = (const Clean <$> symbol ".") <|> (const Infected <$> symbol "#")
