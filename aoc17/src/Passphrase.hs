module Passphrase (
    solve
  , isValid
  , Anagrams
  , buildAnagrams
) where

import qualified Data.Map.Strict as M

import Lib (Part(..), buildCountMap)


solve :: Part -> String -> Int
solve pa = length . filter (== True) . map (isValid pa) . lines

isValid :: Part -> String -> Bool
isValid pa inp = case pa of
  Part1 -> isValid' . words $ inp
  Part2 -> isValid' . map buildAnagrams . words $ inp
 where
  isValid' :: (Foldable t, Ord a) => t a -> Bool
  isValid' = M.null . M.filter ((> 1) :: Int -> Bool) . buildCountMap


-- all possible anagrams
type Anagrams = M.Map Char Int

buildAnagrams :: String -> Anagrams
buildAnagrams = buildCountMap
