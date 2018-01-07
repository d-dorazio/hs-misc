module CoprocessorConflagration (
    solve
  , Register
  , Value (..)
  , Instruction (..)
  , Coprocessor (..)
  , mkCoprocessor
  , modifyCoproIp
  , updateReg
  , readValue
  , part1Exec
  , insParser
  , regParser
  , valueParser
) where

import Control.Applicative ((<|>), some)

import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Text.Megaparsec.Char (letterChar)

import Lib (Part(..), Parser, parseOrPrettyErr, signedDecimal, lexeme, symbol)

solve :: Part -> String -> String
solve Part1 = parseOrPrettyErr (some insParser) (show . mulUntilError (0 :: Int) . mkCoprocessor)
 where
  mulUntilError n copr = case part1Exec copr of
    Nothing           -> n
    Just (copr', mul) -> let n' = if mul then n + 1 else n in mulUntilError n' copr'

solve Part2 =
  let b = 57 * 100 + 100000
  in  const . show . length . filter (not . isPrime) $ [b, b + 17 .. b + 17000]
 where
  isPrime :: Int -> Bool
  isPrime n =
    null . filter (\m -> n `mod` m == 0) $ [2 .. ceiling (sqrt (fromIntegral n) :: Float)]


type Register = Char
data Value = Immediate Int | RegValue Register deriving (Show)
data Instruction = Set Register Value
  | Sub Register Value
  | Mul Register Value
  | Jump Value Value
  deriving (Show)

data Coprocessor = Coprocessor
    { _coproRegs :: M.Map Register Int
    , _coproInstructions :: V.Vector Instruction
    , _coproIp :: Int
    } deriving (Show)

mkCoprocessor :: [Instruction] -> Coprocessor
mkCoprocessor ins =
  Coprocessor {_coproRegs = M.empty, _coproInstructions = V.fromList ins, _coproIp = 0}

modifyCoproIp :: (Int -> Int) -> Coprocessor -> Coprocessor
modifyCoproIp f sc = sc { _coproIp = f (_coproIp sc) }

updateReg :: (Int -> Int) -> Coprocessor -> Register -> Coprocessor
updateReg f sc reg =
  let regs' = M.alter (Just . f . fromMaybe 0) reg . _coproRegs $ sc in sc { _coproRegs = regs' }

readValue :: Coprocessor -> Value -> Int
readValue _  (Immediate imm) = imm
readValue sc (RegValue  reg) = M.findWithDefault 0 reg . _coproRegs $ sc

part1Exec :: Coprocessor -> Maybe (Coprocessor, Bool)
part1Exec isc = let mins = (_coproInstructions isc) V.!? (_coproIp isc) in fmap (exec' isc) mins
 where
  exec' sc (Set reg val) = (incIp (execBinOp const sc reg val), False)
  exec' sc (Sub reg val) = (incIp (execBinOp (flip (-)) sc reg val), False)
  exec' sc (Mul reg val) = (incIp (execBinOp (*) sc reg val), True)
  exec' sc (Jump val off)
    | readValue sc val /= 0 = (modifyCoproIp ((+) (readValue sc off)) sc, False)
    | otherwise             = (incIp sc, False)

  execBinOp fn sc reg val = updateReg (fn (readValue sc val)) sc reg
  incIp sc | _coproIp sc == _coproIp isc = modifyCoproIp (1 +) sc
           | otherwise                   = sc

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
insParser :: Parser Instruction
insParser = binop "set" Set <|> binop "sub" Sub <|> binop "mul" Mul <|> jumpP
 where
  binop iden ctor = do
    _ <- symbol iden
    ctor <$> regParser <*> valueParser

  jumpP = do
    _ <- symbol "jnz"
    Jump <$> valueParser <*> valueParser

regParser :: Parser Register
regParser = lexeme letterChar

valueParser :: Parser Value
valueParser = (RegValue <$> regParser) <|> (Immediate <$> signedDecimal)
