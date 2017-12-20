module DiskFrag (
    solve
  , regionsCount
) where

import Data.Word (Word8)

import Control.Monad (foldM, forM_, when)
import Control.Monad.ST (runST)

import Data.Bits (popCount)

import qualified Data.Vector.Mutable as VM

import Lib (Part(..), stripWs, bits)
import KnotHash (Digest, hashBytesWithSalt)


solve :: Part -> String -> Int
solve pa inp =
  let key     = stripWs inp
      keys    = map (\i -> key ++ "-" ++ show i) ([0 .. 127] :: [Int])
      digests = map hashBytesWithSalt keys
  in  case pa of
        Part1 -> sum . map popCount . concat $ digests
        Part2 -> regionsCount digests


-- Dead == bit turned off
-- Occupied == we have already processed the cell and it has a group
-- Free == the algorithm didn't process this cell yet even if it's set
data Cell = Dead | Occupied Int | Free deriving (Show)


regionsCount :: [Digest] -> Int
regionsCount digests = runST $ do
  mat <- VM.replicate (128 * 128) Dead

  let digestsBits = map (concatMap (bits . (fromIntegral :: Int -> Word8))) digests

  let mkIx        = \row col -> row * 128 + col
  let validIx     = \row col -> and . map (\i -> i >= 0 && i < 128) $ [row, col]

  forM_ (indexed digestsBits) $ \(row, bs) -> do
    forM_ (indexed bs) $ \(col, b) -> do
      when b $ VM.write mat (mkIx row col) Free

  let updateCell = \row col gro -> do
        when (validIx row col) $ do
          cell <- VM.read mat (mkIx row col)

          let updateSiblings = do
                updateCell row       (col + 1) gro
                updateCell row       (col - 1) gro
                updateCell (row + 1) col       gro
                updateCell (row - 1) col       gro

          case cell of
            Dead       -> return ()
            Occupied _ -> return ()
            Free       -> do
              VM.write mat (mkIx row col) (Occupied gro)
              updateSiblings

  let updateGroup = \rowGroup row -> do
        let foo = \gro col -> do
              cell <- VM.read mat (mkIx row col)
              case cell of
                Dead       -> return gro
                Occupied _ -> return gro
                Free       -> do
                  updateCell row col gro
                  return (gro + 1)

        foldM foo rowGroup [0 .. 127]

  foldM updateGroup 0 [0 .. 127]
  where indexed = zip [0 ..]
