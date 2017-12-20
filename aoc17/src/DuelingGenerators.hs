module DuelingGenerators (
    solve
  , Generator (..)
  , low16BytesMatchCount
  , nextValues
  , nextValue
) where

import Data.Bits ((.&.))

import Lib (Part(..))


solve :: Part -> Int
solve Part1 = low16BytesMatchCount . take 40000000 $ zip generatorAValues generatorBValues
 where
  generatorAValues = nextValues generatorA generatorASeed
  generatorBValues = nextValues generatorB generatorBSeed

solve Part2 = low16BytesMatchCount . take 5000000 $ zip generatorAValues generatorBValues
 where
  generatorAValues = filter (\n -> n `mod` 4 == 0) $ nextValues generatorA generatorASeed
  generatorBValues = filter (\n -> n `mod` 8 == 0) $ nextValues generatorB generatorBSeed


newtype Generator = Generator { _genFactor :: Int } deriving (Show)

low16BytesMatchCount :: [(Int, Int)] -> Int
low16BytesMatchCount = length . filter (\(a, b) -> (a .&. 0xFFFF) == (b .&. 0xFFFF))

nextValues :: Generator -> Int -> [Int]
nextValues gen seed = tail . scanl (\s _ -> gen `nextValue` s) seed $ ([0 ..] :: [Int])

nextValue :: Generator -> Int -> Int
nextValue (Generator fac) seed = (fac * seed) `mod` 2147483647


generatorA :: Generator
generatorA = Generator 16807

generatorASeed :: Int
generatorASeed = 516

generatorB :: Generator
generatorB = Generator 48271

generatorBSeed :: Int
generatorBSeed = 190
