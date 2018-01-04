module Lib where

import Control.Applicative

import Data.Bits (Bits, FiniteBits, finiteBitSize, testBit, shiftL, (.|.))
import Data.Function (on)
import Data.List (sortBy, transpose)
import Data.Void

import qualified Data.Map as M
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


--------------------------------------------------------------------
-- Core
--------------------------------------------------------------------
data Part = Part1 | Part2 deriving (Show)


--------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------
buildCountMap :: (Foldable f, Ord a) => f a -> M.Map a Int
buildCountMap = foldr (M.alter incCount) M.empty
 where
  incCount :: Maybe Int -> Maybe Int
  incCount Nothing  = Just 1
  incCount (Just c) = Just (c + 1)

sortByLeastCommon :: (Foldable f, Ord a) => f a -> [a]
sortByLeastCommon = map fst . sortBy (compare `on` snd) . M.toList . buildCountMap

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (pre, post) = splitAt n xs in pre : chunksOf n post

minmax :: [Int] -> Maybe (Int, Int)
minmax = foldr go Nothing
 where
  go :: Int -> Maybe (Int, Int) -> Maybe (Int, Int)
  go x Nothing         = Just (x, x)
  go x (Just (mi, ma)) = Just (min mi x, max ma x)

stripWs :: String -> String
stripWs = T.unpack . T.strip . T.pack

bits :: (FiniteBits b) => b -> [Bool]
bits b = map (testBit b) $ reverse [0 .. finiteBitSize b - 1]

bitsToNum :: (Num b, Bits b) => [Bool] -> b
bitsToNum = foldr (.|.) 0 . zipWith (flip shiftL) [0 ..] . reverse . map (\b -> if b then 1 else 0)

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

rotateCw :: [[a]] -> [[a]]
rotateCw = transpose . reverse

rotations :: [[a]] -> [[[a]]]
rotations grid = scanr (const rotateCw) grid ([1 .. 3] :: [Int])

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
type Parser = Parsec Void String

parseOrPrettyErr :: Parser a -> (a -> String) -> String -> String
parseOrPrettyErr par f inp = case parse par "<stdin>" inp of
  Left  e -> parseErrorPretty e
  Right x -> f x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

decimal :: Parser Int
decimal = lexeme L.decimal

signedDecimal :: Parser Int
signedDecimal = L.signed spaceConsumer decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")
