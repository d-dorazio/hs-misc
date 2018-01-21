module PacketScanner (
    solve
  , Layer (..)
  , bestInitialDelay
  , buildLayersMap
  , genLayersPos
  , layerSeverity
  , severity
  , scannerPos
  , layerParser
  , scannerCaughtUs
) where

import qualified Data.Map as M

import Text.Megaparsec (many)

import Lib (Part(..), Parser, parseOrPrettyErr, decimal, symbol)

solve :: Part -> String -> String
solve pa = parseOrPrettyErr (many layerParser) $ \layers -> case pa of
  Part1 -> show . severity . genLayersPos 0 . buildLayersMap $ layers
  Part2 -> show . bestInitialDelay . buildLayersMap $ layers


type TimePoint = Int
type LayersMap = M.Map Int Layer
data Layer = Layer { _layerDepth :: Int, _layerRange :: Int } deriving (Show)

buildLayersMap :: [Layer] -> LayersMap
buildLayersMap lay = M.fromList . zip (map _layerDepth lay) $ lay

severity :: [(Layer, Int)] -> Int
severity = sum . map (layerSeverity . fst) . filter ((== 0) . snd)

layerSeverity :: Layer -> Int
layerSeverity (Layer dep ran) = dep * ran

genLayersPos :: Int -> LayersMap -> [(Layer, Int)]
genLayersPos baseTime layers = zipWith (\l t -> (l, scannerPos l (baseTime + t)))
                                       (M.elems layers)
                                       ts
  where ts = filter (`M.member` layers) [0 ..]

-- was fast enough :)
bestInitialDelay :: LayersMap -> Int
bestInitialDelay lay = go 0
 where
  go baseTime | scannerCaughtUs (genLayersPos baseTime lay) = go (baseTime + 1)
              | otherwise = baseTime

scannerCaughtUs :: [(Layer, Int)] -> Bool
scannerCaughtUs = any ((== 0) . snd)

scannerPos :: Layer -> TimePoint -> Int
scannerPos (Layer _ ran) t =
  let offset = t `mod` ((ran - 1) * 2)
  in  if offset < ran then offset else ran - (offset `mod` (ran - 1)) - 1

layerParser :: Parser Layer
layerParser = do
  dep   <- decimal
  _     <- symbol ":"
  range <- decimal
  return $ Layer dep range
