{-# LANGUAGE BangPatterns #-}

module TwistyTrampolines (
  solve
) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Lib (Part(..))


solve :: Part -> String -> Int
solve part inp = runST $ do
  jumps <- (V.unsafeThaw . V.fromList . map read . lines) inp
  solve' inc 0 jumps
 where
  inc = case part of
    Part1 -> (1 +)
    Part2 -> (\c -> if c >= 3 then c - 1 else c + 1)


solve' :: (PrimMonad m) => (Int -> Int) -> Int -> VM.MVector (PrimState m) Int -> m Int
solve' inc = go 0
 where
  go :: (PrimMonad m) => Int -> Int -> VM.MVector (PrimState m) Int -> m Int
  go !counter !ix !jumps
    | ix < 0 || ix >= VM.length jumps = return counter
    | otherwise = do
      cur <- VM.unsafeRead jumps ix
      VM.unsafeWrite jumps         ix         (inc cur)
      go             (counter + 1) (ix + cur) jumps
