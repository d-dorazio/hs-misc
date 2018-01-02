module Tubes (
    solve
  , Pos
  , Cell (..)
  , RouteMap (..)
  , navigate
  , routeMapParser
  , cellParser
) where

import Control.Applicative ((<|>), many)

import qualified Data.Map.Strict as M

import Text.Megaparsec (getPosition, sourceLine, sourceColumn, unPos)
import Text.Megaparsec.Char (letterChar)

import Lib (Part(..), Parser, parseOrPrettyErr, lexeme, symbol, spaceConsumer)


solve :: Part -> String -> String
solve part = parseOrPrettyErr routeMapParser $ \routesMap -> case part of
  Part1 -> fst . navigate $ routesMap
  Part2 -> show . snd . navigate $ routesMap


type Pos = (Int, Int)
data Cell = Straight | Turn | Point Char deriving (Show)
newtype RouteMap = RouteMap (M.Map Pos Cell) deriving (Show)

navigate :: RouteMap -> (String, Int)
navigate (RouteMap routes) = go entryPos entryDir (1, 0) 1 ""
 where
  [(entryPos, entryDir)] = M.toList . M.filterWithKey (\(r, _) _ -> r == 1) $ routes

  go pos (Point ch) step seen acc = go pos Straight step seen (acc ++ [ch])

  go (row, col) Straight (dr, dc) seen acc =
    let pos' = (row + dr, col + dc)
    in  case M.lookup pos' routes of
          Just dir' -> go pos' dir' (dr, dc) (seen + 1) acc
          Nothing   -> (acc, seen)

  go (row, col) Turn (dr, dc) seen acc =
    let [(pos', dir', step')] =
          [ (p, d, (dr', dc'))
          | dr' <- if dr /= 0 then [0] else [-1, 1]
          , dc' <- if dc /= 0 then [0] else [-1, 1]
          , let p = (row + dr', col + dc')
          , Just d <- [M.lookup p routes]
          ]
    in  go pos' dir' step' (seen + 1) acc


--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
routeMapParser :: Parser RouteMap
routeMapParser = do
  _  <- spaceConsumer
  kv <- many route
  return . RouteMap . M.fromList $ kv
 where
  route = do
    pos <- getPosition
    dir <- cellParser
    let row = unPos (sourceLine pos)
    let col = unPos (sourceColumn pos)
    return ((row, col), dir)


cellParser :: Parser Cell
cellParser = straightP <|> turnP <|> pointP
 where
  straightP = const Straight <$> (symbol "-" <|> symbol "|")
  turnP     = const Turn <$> symbol "+"
  pointP    = Point <$> lexeme letterChar
