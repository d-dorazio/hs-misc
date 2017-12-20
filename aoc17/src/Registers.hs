module Registers (
    solve
  , instructionsParser
  , Cpu (..)
  , Change (..)
  , If (..)
  , Op (..)
  , BoolOp (..)
  , maxReg
  , getCpuReg
  , updateCpuReg
  , exec
  , predPass
  , changeReg
) where

import qualified Data.Map as M

import Text.Megaparsec (some, try, (<|>))
import Text.Megaparsec.Char (letterChar)

import Lib (Part(..), Parser, parseOrPrettyErr, lexeme, signedDecimal, symbol)


solve :: Part -> String -> String
solve pa = parseOrPrettyErr instructionsParser $ \ins ->
  let (cpu, maxRegEver) = foldl (flip exec) (newCpu, 0) ins
  in  case pa of
        Part1 -> show . maxReg $ cpu
        Part2 -> show maxRegEver


type Register = String
newtype Cpu = Cpu { _cpuRegisters :: M.Map Register Int } deriving (Show)


newCpu :: Cpu
newCpu = Cpu M.empty

maxReg :: Cpu -> Int
maxReg = maximum . _cpuRegisters

getCpuReg :: Register -> Cpu -> Int
getCpuReg r (Cpu regs) = M.findWithDefault 0 r regs

updateCpuReg :: (Int -> Int) -> Register -> Cpu -> Cpu
updateCpuReg f r (Cpu regs) = Cpu . M.alter alt r $ regs
 where
  alt Nothing  = Just $ f 0
  alt (Just x) = Just $ f x


data Op = Inc | Dec deriving (Show)
data BoolOp = Eq | Ne | Lt | Le | Gt | Ge deriving (Show)

data Change = Change Register Op Int deriving (Show)
data If = If Register BoolOp Int deriving (Show)

data Instruction = Instruction Change If deriving (Show)

exec :: Instruction -> (Cpu, Int) -> (Cpu, Int)
exec (Instruction cha@(Change chaReg _ _) if_) (cpu, maxRegEver) =
  let cpu'        = if predPass if_ cpu then changeReg cha cpu else cpu
      maxRegEver' = max maxRegEver (getCpuReg chaReg cpu')
  in  (cpu', maxRegEver')


predPass :: If -> Cpu -> Bool
predPass (If reg op val) =
  let predic = case op of
        Eq -> (==)
        Ne -> (/=)
        Lt -> (<)
        Le -> (<=)
        Gt -> (>)
        Ge -> (>=)
  in  (`predic` val) . getCpuReg reg


changeReg :: Change -> Cpu -> Cpu
changeReg (Change reg op val) =
  let upd = case op of
        Inc -> (+)
        Dec -> (-)
  in  updateCpuReg (`upd` val) reg

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
instructionsParser :: Parser [Instruction]
instructionsParser = some instructionParser

instructionParser :: Parser Instruction
instructionParser = do
  change <- changeParser
  _      <- symbol "if"
  if_    <- ifParser

  return $ Instruction change if_

changeParser :: Parser Change
changeParser = Change <$> regParser <*> opParser <*> signedDecimal

ifParser :: Parser If
ifParser = If <$> regParser <*> boolOpParser <*> signedDecimal

regParser :: Parser Register
regParser = lexeme $ some letterChar

opParser :: Parser Op
opParser = (symbol "inc" >> return Inc) <|> (symbol "dec" >> return Dec)

boolOpParser :: Parser BoolOp
boolOpParser =
  (symbol "==" >> return Eq)
    <|> (symbol "!=" >> return Ne)
    <|> try (symbol "<=" >> return Le)
    <|> (symbol "<" >> return Lt)
    <|> try (symbol ">=" >> return Ge)
    <|> (symbol ">" >> return Gt)
