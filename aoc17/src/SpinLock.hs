{-# LANGUAGE BangPatterns #-}

module SpinLock (
    solve
  , Buffer
  , firstAfter
  , spinLockRun
) where

import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

import Lib (Part(..))


solve :: Part -> String -> Int
solve Part1 = fromJust . firstAfter 2017 . spinLockRun 2017 . read
solve Part2 = fromJust . firstAfter 0 . spinLockRun 50000000 . read -- fast enough


type Buffer = Seq.Seq Int

firstAfter :: Int -> Buffer -> Maybe Int
firstAfter val buf =
  fmap (\i -> Seq.index buf ((i + 1) `mod` (Seq.length buf))) $ Seq.elemIndexL val buf

spinLockRun :: Int -> Int -> Buffer
spinLockRun size steps = go 0 (Seq.singleton 0)
 where
  go !pos !buf
    | Seq.length buf == size + 1
    = buf
    | otherwise
    = let bufLen = Seq.length buf
          pos'   = ((pos + steps) `mod` bufLen) + 1
          buf'   = Seq.insertAt pos' bufLen buf
      in  go pos' buf'
