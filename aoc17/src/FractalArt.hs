module FractalArt (
    solve
  , Grid
  , Pixel (..)
  , EnhancementRule
  , EnhancementRules (..)
  , step
  , mapSubSquares
  , buildEnhancementRules
  , similarGrids
  , enhance
  , enhancementRuleParser
  , gridParser
) where

import Control.Applicative ((<|>), some)

import Data.List (transpose)

import qualified Data.Map.Strict as M

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

import Lib (Part(..), Parser, parseOrPrettyErr, lexeme, symbol, chunksOf, spaceConsumer, rotations)


-- oh boy, this was a pain...

solve :: Part -> String -> String
solve part = parseOrPrettyErr (some enhancementRuleParser) $ \rules ->
  let erules = buildEnhancementRules rules
      img    = foldr (const (step erules)) initialGrid ([1 .. iterations] :: [Int])
  in  show . countOn $ img
 where
  iterations = case part of
    Part1 -> 5
    Part2 -> 18

  initialGrid = [[Off, On, Off], [Off, Off, On], [On, On, On]]
  countOn     = length . filter (== On) . concat


type Grid = [[Pixel]]
data Pixel = On | Off deriving (Eq, Ord, Show)

type EnhancementRule = (Grid, Grid)
newtype EnhancementRules = EnhancementRules (M.Map Grid Grid) deriving (Show)

step :: EnhancementRules -> Grid -> Grid
step rules grid = mapSubSquares (enhance rules) grid

mapSubSquares :: (Grid -> Grid) -> Grid -> Grid
mapSubSquares rules xs = chunksOf n xs >>= map concat . transpose . map rules . transpose . map
  (chunksOf n)
 where
  n | even (length xs) = 2
    | otherwise        = 3

buildEnhancementRules :: [EnhancementRule] -> EnhancementRules
buildEnhancementRules rules =
  EnhancementRules . M.fromList $ [ (fp, to) | (f, to) <- rules, fp <- similarGrids f ]

similarGrids :: [[a]] -> [[[a]]]
similarGrids f = rotations f ++ rotations (reverse f)

enhance :: EnhancementRules -> Grid -> Grid
enhance (EnhancementRules rules) grid = rules M.! grid

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
enhancementRuleParser :: Parser EnhancementRule
enhancementRuleParser = do
  from <- lexeme gridParser
  spaceConsumer
  _  <- symbol "=>"
  to <- lexeme gridParser
  return (from, to)

gridParser :: Parser Grid
gridParser = (some pixelParser) `sepBy` symbol "/"
  where pixelParser = (const On <$> char '#') <|> (const Off <$> char '.')

