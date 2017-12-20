module Captcha (
  solve
) where

import Lib (Part(..))

-- vectors would allow better performance probably
-- because of the O(1) member access, but the rotate
-- trick is cool and performance is not that bad.

solve :: Part -> String -> Int
solve part inp = solve' step xs
 where
  xs   = map (read . (: "")) . init $ inp
  step = case part of
    Part1 -> 1
    Part2 -> length xs `div` 2

solve' :: Int -> [Int] -> Int
solve' step inp = sum . map fst . filter (uncurry (==)) $ paired
 where
  paired :: [(Int, Int)]
  paired = zip inp (drop step (cycle inp))
