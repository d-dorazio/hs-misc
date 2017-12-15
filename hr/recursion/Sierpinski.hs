#!/usr/bin/env stack
-- stack --resolver lts-9.2 script

-- really cool

sierpinski
  :: (Int, Int) -> Int -> [String]
sierpinski (w, h) 0 = take h . map row $ [1, 3 ..]
 where
  row :: Int -> String
  row r =
    let mw   = w `quot` 2
        midr = r `quot` 2
        sr   = replicate (mw - midr) '_' ++ replicate midr '1'
    in  sr ++ ['1'] ++ reverse sr

sierpinski (w, h) n = top ++ bottom
 where
  top        = map (\t -> topPadding ++ t ++ topPadding) unit
  bottom     = zipWith (\bl br -> bl ++ ('_' : br)) unit unit
  unit       = sierpinski (mw, mh) (n - 1)

  topPadding = replicate ((mw `quot` 2) + 1) '_'
  mh         = h `quot` 2
  mw         = w `quot` 2

main = do
  n <- readLn
  putStrLn . unlines . sierpinski (63, 32) $ n
