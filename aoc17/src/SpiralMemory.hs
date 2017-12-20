module SpiralMemory(
    solve
  , solve'
  , createMatrix
  , prettyMatrix
) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)

import Data.Maybe (fromJust)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Lib (Part(..))


-- my solution is quite inefficient because it always calculates the whole matrix while
-- it should calculate numbers up until the conditions holds, but it's fast enough...
solve :: Part -> String -> Int
solve pa inp = fromJust $ solve' pa inp' (createMatrix pa n)
 where
  n :: Int
  n = let s = ceiling (sqrt (fromIntegral inp') :: Float) in if even s then s + 1 else s

  inp' :: Int
  inp' = read inp


prettyMatrix :: V.Vector (VU.Vector Int) -> String
prettyMatrix = concatMap (\v -> unwords (map (rjust 4 ' ' . show) (VU.toList v)) ++ "\n")
 where
  rjust n c l = let len = length l in if len >= n then take n l else replicate (n - len) c ++ l


createMatrix :: Part -> Int -> V.Vector (VU.Vector Int)
createMatrix part size = runST $ do
  mat <- V.replicateM size (VUM.replicate size 0)

  VUM.write (mat V.! center) center 1
  fill      1                2      mat

  V.mapM VU.unsafeFreeze mat
 where
  center = size `div` 2

  fill ring num mat
    | ring > center = return ()
    | otherwise = do
      let ncells = ring * 8
      let side   = ncells `div` 4

      forM_ [1 .. ncells] $ \i -> do
        let (x, y) = ccwPos ring side i

        val <- case part of
          Part1 -> return $ num + i - 1
          Part2 -> do
            let neighborsPos =
                  [ (y', x')
                  | y' <- [y - 1 .. y + 1]
                  , x' <- [x - 1 .. x + 1]
                  , y' >= 0
                  , x' >= 0
                  , y' < size
                  , x' < size
                  ]
            neighbors <- mapM (\(y', x') -> VUM.read (mat V.! y') x') neighborsPos
            return $ sum neighbors

        VUM.write (mat V.! y) x val

      fill (ring + 1) (num + ncells) mat

  -- counter clockwise position
  -- i is 1based
  ccwPos ring side i | i <= side     = (center + ring, center + ring - i)
                     | i <= side * 2 = (center + ring - (i - side), center - ring)
                     | i <= side * 3 = (center - ring, center - ring + (i - side * 2))
                     | i <= side * 4 = (center - ring + (i - side * 3), center + ring)
                     | otherwise     = error "bug"


solve' :: Part -> Int -> V.Vector (VU.Vector Int) -> Maybe Int
solve' Part2 val vec = V.foldr go Nothing vec
 where
  go :: VU.Vector Int -> Maybe Int -> Maybe Int
  go row acc =
    let row' = VU.filter (> val) row
    in  case (row', acc) of
          (v , Nothing) | VU.null v -> Nothing
          (v , Just m ) | VU.null v -> Just m
          (ms, Nothing)             -> Just . VU.minimum $ ms
          (ms, Just m )             -> Just . min m $ VU.minimum ms

solve' Part1 val vec = V.ifoldr go Nothing $ vec
 where
  center :: Int
  center = V.length vec `div` 2

  go :: Int -> VU.Vector Int -> Maybe Int -> Maybe Int
  go _ _ (Just d) = Just d
  go y v Nothing  = case VU.ifoldr acc Nothing v of
    Just x  -> Just (abs (center - x) + abs (center - y))
    Nothing -> Nothing

  acc :: Int -> Int -> Maybe Int -> Maybe Int
  acc _ _ (Just j) = Just j
  acc x n Nothing  = if n == val then Just x else Nothing
