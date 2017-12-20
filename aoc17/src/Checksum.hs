module Checksum (
    solve
) where

import Data.Maybe (fromJust)

import Lib (Part(..), minmax)

solve :: Part -> String -> Int
solve part = solve' part . map (map read . words) . lines


solve' :: Part -> [[Int]] -> Int
solve' part = sum . map (f part)
 where
  f :: Part -> [Int] -> Int
  f Part1 l = uncurry (flip (-)) . fromJust . minmax $ l
  f Part2 l = head [ x `div` y | x <- l, y <- l, x /= y, x `mod` y == 0 ]
