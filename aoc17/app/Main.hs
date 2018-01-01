import Data.Semigroup ((<>))
import Options.Applicative

import Lib

import Captcha
import Checksum
import Circus
import SpiralMemory
import Passphrase
import TwistyTrampolines
import MemoryReallocation
import Registers
import Stream
import KnotHash
import HexEd
import DigitalPlumber
import PacketScanner
import DiskFrag
import DuelingGenerators
import PermutationPromenade
import SpinLock


data Cli = Cli {
    _day :: Int
 , _part :: Part
} deriving (Show)


cliParser :: Options.Applicative.Parser Cli
cliParser = Cli <$> option auto (long "day" <> short 'd' <> help "advent of code day") <*> option
  (maybeReader partParser)
  (long "part" <> short 'p' <> help "part number")
 where
  partParser "1" = Just Part1
  partParser "2" = Just Part2
  partParser _   = Nothing


main :: IO ()
main = do
  let opts = info (cliParser <**> helper) (fullDesc <> progDesc "advent of code 2017 in haskell")
  cli <- execParser opts

  inp <- getContents

  let fn = solutions !! (_day cli - 1)
  putStrLn . fn (_part cli) $ inp
 where
  showFn f = \pa inp -> show $ f pa inp
  solutions =
    [ showFn Captcha.solve
    , showFn Checksum.solve
    , showFn SpiralMemory.solve
    , showFn Passphrase.solve
    , showFn TwistyTrampolines.solve
    , showFn MemoryReallocation.solve
    , Circus.solve
    , Registers.solve
    , Stream.solve
    , KnotHash.solve
    , HexEd.solve
    , DigitalPlumber.solve
    , PacketScanner.solve
    , showFn DiskFrag.solve
    , \pa _ -> show . DuelingGenerators.solve $ pa
    , PermutationPromenade.solve
    , showFn SpinLock.solve
    ]
