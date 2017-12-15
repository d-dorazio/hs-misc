#!/usr/bin/env stack
-- stack --resolver lts-9.2 script
import qualified Data.Set as Set

-- quite nice indeed!

type Pos = (Int, Int)


diag :: Pos -> Int
diag (r, c) = r - c

antiDiag :: Pos -> Int
antiDiag (r, c) = c + r


superQueens :: Int -> [[Pos]]
superQueens = superQueens' (1000, 1000) 1 Set.empty Set.empty Set.empty


superQueens' :: (Int, Int) -> Int -> Set.Set Int -> Set.Set Int -> Set.Set Int -> Int -> [[Pos]]
superQueens' (lastC, secondLastC) nRow columns diags antiDiags n
  | nRow > n  = [[]]
  | otherwise = concatMap (\col -> map ((nRow, col):) $ subSuperQueens col) queensCols
 where
  queensCols :: [Int]
  queensCols =
    [ c
    | c <- [1 .. n]
    , abs (c - lastC) /= 2
    , abs (c - secondLastC) /= 1
    , columns `notContains` c
    , diags `notContains` (diag (nRow, c))
    , antiDiags `notContains` (antiDiag (nRow, c))
    ]

  subSuperQueens :: Int -> [[Pos]]
  subSuperQueens c =
    let nRow'      = nRow + 1
        cols'      = Set.insert c columns
        diags'     = Set.insert (diag (nRow, c)) diags
        antiDiags' = Set.insert (antiDiag (nRow, c)) antiDiags
    in  superQueens' (c, lastC) nRow' cols' diags' antiDiags' n


notContains :: (Ord a) => Set.Set a -> a -> Bool
notContains = flip Set.notMember


main :: IO ()
main = do
  n <- readLn
  print . length . superQueens $ n
