module ElectroMagneticMoat (
    solve
  , Bridge
  , Port
  , mkPortMap
  , bridges
  , score
  , portParser
) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Text.Megaparsec (some)

import Lib (Part(..), Parser, parseOrPrettyErr, symbol, decimal)

solve :: Part -> String -> String
solve pa = parseOrPrettyErr (some portParser) $ \ports ->
  let portMap = mkPortMap ports
      brid    = bridges portMap 0 S.empty
  in  case pa of
        Part1 -> show . maximum . map score $ brid
        Part2 -> show . snd . maximum . map (\b -> (S.size b, score b)) $ brid

type Bridge = S.Set Port
type Port = (Int, Int)

mkPortMap :: [Port] -> M.Map Int (S.Set Int)
mkPortMap = foldr (\(f, t) -> appendPort f t . appendPort t f) M.empty
  where appendPort k v = M.insertWith S.union k (S.singleton v)

bridges :: M.Map Int (S.Set Int) -> Int -> Bridge -> [Bridge]
bridges portMap start bridge = case portMap M.!? start of
  Nothing          -> []
  Just connections -> bridge : foldr recur [] connections
 where
  recur end acc =
    let port = if start >= end then (start, end) else (end, start)
    in  if S.member port bridge then acc else acc ++ bridges portMap end (S.insert port bridge)

score :: Foldable t => t Port -> Int
score ps = foldr (\(f, t) acc -> acc + f + t) 0 ps


--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------

portParser :: Parser Port
portParser = do
  from <- portP
  _    <- symbol "/"
  to   <- portP
  return (from, to)
  where portP = decimal
