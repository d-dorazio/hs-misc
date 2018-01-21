#!/usr/bin/env stack
-- stack --resolver lts-10.3 script

-- nice!!

fracTree :: (Int, Int) -> Int -> Int -> [String]
fracTree (w, h) branchSize 1 = top ++ center ++ bottom
 where
  top    = replicate (h - branchSize * 2) . replicate w $ '_'

  center = reverse . take branchSize . map centerRow $ [0 ..]
  centerRow i =
    let lbranch = replicate (mw - 2 - i) '_' ++ "1"
        rbranch = "1" ++ replicate (mw - 1 - i) '_'
    in  lbranch ++ replicate (i * 2 + 1) '_' ++ rbranch

  bottom     = replicate branchSize bottomLine
  bottomLine = replicate (mw - 1) '_' ++ ('1' : replicate mw '_')

  (mw, _)    = (w `quot` 2, h `quot` 2)

fracTree (w, h) branchSize n = top ++ bottom
 where
  top        = zipWith (\l r -> topPadding ++ l ++ r ++ topPadding) unit unit
  unit       = fracTree (branchSize * 2, mh) (branchSize `quot` 2) (n - 1)
  topPadding = if branchSize == 16 then replicate (branchSize + 2) '_' else []

  bottom     = fracTree (w, mh) branchSize 1

  (_, mh)    = (w `quot` 2, h `quot` 2)

main :: IO ()
main = do
  n <- readLn
  putStrLn . unlines . fracTree (100, 63) 16 $ n
