module Circus (
    solve
  , Tower (..)
  , Program (..)
  , buildTower
  , towerRoots
  , towerParser
  , programParser
  , towerBalance
) where

import Control.Applicative

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (letterChar)

import Lib (
    Part(..)
  , Parser
  , parseOrPrettyErr
  , decimal
  , lexeme
  , parens
  , symbol
  , sortByLeastCommon
  )


solve :: Part -> String -> String
solve part = parseOrPrettyErr towerParser $ \programs ->
  let tower     = buildTower programs
      towerRoot = head . S.toList . towerRoots $ tower
  in  case part of
        Part1 -> towerRoot
        Part2 -> case towerBalance tower (tower `getProg` towerRoot) of
          Balanced w -> "Balanced at " ++ show w
          Unbalanced unb expWeight ->
            let prettyUnb name curWeight =
                  let pr    = tower `getProg` name
                      delta = expWeight - curWeight
                  in  "Unbalanced "
                      ++ name
                      ++ " current subtree weight: "
                      ++ show curWeight
                      ++ " expected weight: "
                      ++ show expWeight
                      ++ " new weight for current node would be: "
                      ++ show (_progWeight pr + delta)
            in  concat . M.elems . M.mapWithKey prettyUnb $ unb


newtype Tower = Tower (M.Map String Program) deriving (Show)

data Program = Program {
    _progName :: String
 ,  _progWeight :: Int
 , _progChildren :: S.Set String
} deriving (Show)

buildTower :: [Program] -> Tower
buildTower = Tower . foldr (\prog acc -> M.insert (_progName prog) prog acc) M.empty

getProg :: Tower -> String -> Program
getProg (Tower t) = (M.!) t

towerRoots :: Tower -> S.Set String
towerRoots (Tower tower) = M.keysSet nonLeaves `S.difference` nonLeavesChildren
 where
  nonLeavesChildren :: S.Set String
  nonLeavesChildren = foldr (\prog acc -> acc `S.union` _progChildren prog) S.empty nonLeaves

  nonLeaves :: M.Map String Program
  nonLeaves = M.filter (not . null . _progChildren) tower


data Balance = Balanced Int | Unbalanced (M.Map String Int) Int deriving (Show)

isUnbalanced :: Balance -> Bool
isUnbalanced (Balanced _    ) = False
isUnbalanced (Unbalanced _ _) = True

towerBalance :: Tower -> Program -> Balance
towerBalance tower prog = case M.filter isUnbalanced childrenBalance of
  unb | not (M.null unb) -> head . M.elems $ unb
  _                      -> balance
 where
  childrenBalance :: M.Map String Balance
  childrenBalance = M.map (towerBalance tower) childrenProgs

  balance :: Balance
  balance
    | M.null childrenProgs
    = Balanced progWeight
    | otherwise
    = let childrenWeights  = M.map (\(Balanced w) -> w) childrenBalance
          mostCommonWeight = last . sortByLeastCommon . M.elems $ childrenWeights
          unbalancedProgs  = M.filter (/= mostCommonWeight) childrenWeights
      in  if null unbalancedProgs
            then Balanced (progWeight + mostCommonWeight * M.size childrenProgs)
            else Unbalanced unbalancedProgs mostCommonWeight

  progWeight = _progWeight prog
  childrenProgs =
    foldr (\chi acc -> M.insert chi (tower `getProg` chi) acc) M.empty . _progChildren $ prog


--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
towerParser :: Parser [Program]
towerParser = some programParser

programParser :: Parser Program
programParser = Program <$> iden <*> parens decimal <*> (children <|> return S.empty)
 where
  iden     = lexeme $ some letterChar
  children = do
    _ <- symbol "->"
    S.fromList <$> iden `sepBy` symbol ", "
