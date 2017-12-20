module DigitalPlumber (
    solve
  , ProgId
  , Program(..)
  , distinctGroups
  , connectedTo
  , buildProgsMap
  , progParser
) where


import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Text.Megaparsec (many, sepBy)

import Lib (Part(..), parseOrPrettyErr, Parser, decimal, symbol)


solve :: Part -> String -> String
solve pa = parseOrPrettyErr progsParser $ \progs -> case pa of
  Part1 -> show . S.size . connectedTo 0 . buildProgsMap $ progs
  Part2 -> show . length . distinctGroups S.empty . buildProgsMap $ progs
  where progsParser = many progParser


type ProgId = Int
data Program = Program { _progId :: ProgId, _progsConnected :: S.Set ProgId} deriving (Show)


distinctGroups :: S.Set ProgId -> M.Map ProgId Program -> [S.Set ProgId]
distinctGroups seen allProgs =
  let new = M.keysSet allProgs `S.difference` seen
  in  if S.null new
        then []
        else
          let group = connectedTo (S.findMin new) allProgs
              seen' = seen `S.union` group
          in  group : distinctGroups seen' allProgs


connectedTo :: ProgId -> M.Map ProgId Program -> S.Set ProgId
connectedTo to allProgs = connectedTo' to S.empty
 where
  connectedTo' to' pidSeen =
    let (Program pid connected) = allProgs M.! to'
    in  foldr childrenConnectedTo (S.insert pid pidSeen) connected

  childrenConnectedTo p acc | p `S.notMember` acc = connectedTo' p acc
                            | otherwise           = acc


buildProgsMap :: [Program] -> M.Map ProgId Program
buildProgsMap progs = M.fromList $ zip (map _progId progs) progs

progParser :: Parser Program
progParser = do
  pid       <- decimal
  _         <- symbol "<->"
  connected <- S.fromList <$> decimal `sepBy` symbol ","
  return $ Program pid connected
