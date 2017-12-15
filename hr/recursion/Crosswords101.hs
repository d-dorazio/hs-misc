#!/usr/bin/env stack
-- stack --resolver lts-9.2 script

-- I'm not proud of this, but it worked

import Control.Monad (replicateM)
import Data.List (transpose)

type CWord = String
type CRow = String

solve :: [CWord] -> [CRow] -> [[CRow]]
solve ws rs = map (transpose . fst) sols
 where
  sols       = filter (null . snd) $ vsols
  vsols      = concatMap (\(m, w) -> hsolve w m) transposed
  transposed = map (\(m, w) -> (transpose m, w)) hsols
  hsols      = hsolve ws rs

hsolve :: [CWord] -> [CRow] -> [([CRow], [CWord])]
hsolve [] m      = [(m, [])]
hsolve ws []     = [([], ws)]
hsolve ws (r:rs) = concatMap go . solveRow ws $ r
  where go (r', ws') = map (\(r'', ws'') -> (r' : r'', ws'')) $ hsolve ws' rs


solveRow :: [CWord] -> CRow -> [(CRow, [CWord])]
solveRow ws [] = [("", ws)]
solveRow ws r  = concatMap go matches
 where
  go :: CWord -> [(CRow, [CWord])]
  go w =
    let ws'     = filter (/=w) ws
        subSols = solveRow ws' rest'
    in  map (\(x, rws) -> (pre ++ w ++ x, rws)) subSols

  matches         = if null wordMatches then [toFill] else wordMatches
  wordMatches     = allMatches ws toFill

  (toFill, rest') = break (=='+') rest
  (pre   , rest ) = break (/='+') r


allMatches :: [CWord] -> CRow -> [CWord]
allMatches ws r = filter (\w -> match w r) ws

match :: CWord -> CRow -> Bool
match [] [] = True
match [] _  = False
match _  [] = False
match (wc:wcs) (rc:rcs) | rc == '-' || wc == rc = match wcs rcs
                        | otherwise             = False

main = do
  matrix <- replicateM 10 getLine

  ws     <- fmap (split ';') getLine

  -- mapM_ putStrLn matrix
  -- putStrLn "\n\n"

  let (sol:[]) = solve ws matrix
  mapM_ putStrLn sol

split :: Char -> String -> [String]
split c = splitOn (==c)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> w : splitOn p s'' where (w, s'') = break p s'
