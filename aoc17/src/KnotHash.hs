module KnotHash (
    solve
  , Digest
  , hashBytes
  , hashBytesWithSalt
  , sparseHash
) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl')

import qualified Data.Vector.Unboxed as VU

import Text.Printf (printf)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

import Lib (Part(..), parseOrPrettyErr, decimal, chunksOf, stripWs)


solve :: Part -> String -> String
solve Part1 inp = parseOrPrettyErr (decimal `sepBy` char ',') (show . prodFirst2 . hashBytes) inp
 where
  prodFirst2 (x:y:_) = x * y
  prodFirst2 _       = error "input bug?"

solve Part2 inp = concatMap (printf "%02x") . hashBytesWithSalt $ inp

type Digest = [Int]

hashBytesWithSalt :: String -> Digest
hashBytesWithSalt inp =
  let corpus = map ord . stripWs $ inp
  in  map sparseHash . chunksOf 16 . hashBytes . concat . replicate 64 $ corpus ++ salt
  where salt = [17, 31, 73, 47, 23]


hashBytes :: [Int] -> Digest
hashBytes bytes =
  let (nums, _, _) = foldl' step (VU.enumFromN 0 256, 0, 0) bytes in VU.toList nums
 where
  step (nums, pos, skipSize) len =
    let nums'     = revSubList nums pos len
        pos'      = pos + len + skipSize
        skipSize' = skipSize + 1
    in  (nums', pos', skipSize')

  revSubList nums pos len =
    let ixs = map (`mod` VU.length nums) [pos .. pos + len - 1]
        sub = map (nums VU.!) ixs
    in  (VU.//) nums (zip ixs (reverse sub))


sparseHash :: [Int] -> Int
sparseHash = foldr xor 0
