module HexEd (
    solve
  , Direction(..)
  , moveToDirs
  , move
  , distance
  , dirCoordinates
  , dirParser
) where

import Text.Megaparsec (choice, sepBy, try)
import Text.Megaparsec.Char (char, string)

import Lib (Part(..), parseOrPrettyErr, Parser)


solve :: Part -> String -> String
solve pa = parseOrPrettyErr dirsParser $ \dirs -> case pa of
  Part1 -> show . distance . last . moveToDirs $ dirs
  Part2 -> show . maximum . map distance . moveToDirs $ dirs
  where dirsParser = dirParser `sepBy` (char ',')


data Direction = Nw | N | Ne | Se | S | Sw deriving (Show)


moveToDirs :: [Direction] -> [(Int, Int)]
moveToDirs = scanl move (0, 0)


move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) dir = let (dx, dy) = dirCoordinates dir in (x + dx, y + dy)


distance :: (Int, Int) -> Int
distance (x, y) = (abs x + abs y + abs (x + y)) `div` 2


-- axial coordinates: https://www.redblobgames.com/grids/hexagons/#coordinates-axial
dirCoordinates :: Direction -> (Int, Int)
dirCoordinates Nw = (-1, 1)
dirCoordinates N  = (0, 1)
dirCoordinates Ne = (1, 0)
dirCoordinates Sw = (-1, 0)
dirCoordinates S  = (0, -1)
dirCoordinates Se = (1, -1)


dirParser :: Parser Direction
dirParser = choice
  [ try (string "nw") >> return Nw
  , try (string "ne") >> return Ne
  , char 'n' >> return N
  , try (string "sw") >> return Sw
  , try (string "se") >> return Se
  , char 's' >> return S
  ]
