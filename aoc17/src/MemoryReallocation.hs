module MemoryReallocation (
    solve
  , solve'
  , redistribute
  , Banks
  , BanksCache
) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import qualified Data.Map.Strict as M

import Lib (Part(..))


type Banks = VU.Vector Int
type BanksCache = M.Map Banks Int


solve :: Part -> String -> Int
solve part inp = solve' part 0 M.empty banks
 where
  banks :: Banks
  banks = VU.fromList . map read . words $ inp


solve' :: Part -> Int -> BanksCache -> Banks -> Int
solve' part nsols banksCache banks = case cacheHit of
  Just hitAt -> case part of
    Part1 -> nsols
    Part2 -> nsols - hitAt
  Nothing -> solve' part nsols' banksCache' banks'
 where
  cacheHit    = M.lookup banks banksCache
  nsols'      = nsols + 1
  banksCache' = M.insert banks nsols banksCache
  banks'      = redistribute banks


redistribute :: Banks -> Banks
redistribute banks = runST $ do
  mv <- VU.thaw banks
  go mv
  VU.freeze mv
 where
  go banksm = do
    let mai = VU.maxIndex banks

    ma <- VUM.read banksm mai
    VUM.write banksm mai 0

    let banksLength = VU.length banks
    let ixs         = [ ix `mod` banksLength | ix <- [mai + 1 .. mai + banksLength] ]
    let cycled      = take ma . cycle $ ixs

    forM_ cycled $ VUM.modify banksm (1 +)
