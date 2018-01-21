module Duet (
    solve
  , Register
  , Value (..)
  , Instruction
  , Actor (..)
  , newActor
  , modifyActorIp
  , updateReg
  , readValue
  , actorSend

  , recoverSound
  , soundExecUntilRecover
  , soundExec

  , actor2ExecUntilDeadLock
  , actorExec

  , insParser
  , regParser
  , valueParser
) where

import Control.Applicative ((<|>), some)

import Data.Maybe (fromJust, fromMaybe)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Text.Megaparsec (try)
import Text.Megaparsec.Char (letterChar)

import Lib (Part(..), Parser, parseOrPrettyErr, signedDecimal, lexeme, symbol)


solve :: Part -> String -> String
solve pa = parseOrPrettyErr (some insParser) $ \ins -> case pa of
  Part1 -> show . fromJust . soundExecUntilRecover . newActor $ ins
  Part2 ->
    let ac0 = (newActor ins) { _actorRegs = M.singleton 'p' 0 }
        ac1 = (newActor ins) { _actorRegs = M.singleton 'p' 1 }
    in  show $ actor2ExecUntilDeadLock ac0 ac1


type Register = Char
data Value = Immediate Int | RegValue Register deriving (Show)
data Instruction = Send Value
  | Set Register Value
  | Add Register Value
  | Mul Register Value
  | Mod Register Value
  | Receive Register
  | Jump Value Value
  deriving (Show)

data Actor = Actor
  { _actorRegs :: M.Map Register Int
  , _actorOutChannel :: [Int]
  , _actorInChannel :: [Int]
  , _actorInstructions :: V.Vector Instruction
  , _actorIp :: Int
  } deriving (Show)

newActor :: [Instruction] -> Actor
newActor ins = Actor
  { _actorRegs         = M.empty
  , _actorOutChannel   = []
  , _actorInChannel    = []
  , _actorInstructions = V.fromList ins
  , _actorIp           = 0
  }

modifyActorIp :: (Int -> Int) -> Actor -> Actor
modifyActorIp f sc = sc { _actorIp = f (_actorIp sc) }

updateReg :: (Int -> Int) -> Actor -> Register -> Actor
updateReg f sc reg =
  let regs' = M.alter (Just . f . fromMaybe 0) reg . _actorRegs $ sc in sc { _actorRegs = regs' }

readValue :: Actor -> Value -> Int
readValue _  (Immediate imm) = imm
readValue sc (RegValue  reg) = M.findWithDefault 0 reg . _actorRegs $ sc

actorSend :: Actor -> Int -> Actor
actorSend sc freq = sc { _actorOutChannel = freq : _actorOutChannel sc }

--------------------------------------------------------------------
-- SoundCard(aka Part1)
-- can be seen as actor that reads its outchannel
--------------------------------------------------------------------
recoverSound :: Actor -> Actor
recoverSound sc = case _actorOutChannel sc of
  []           -> sc
  (lastFreq:_) -> sc { _actorInChannel = lastFreq : _actorInChannel sc }


soundExecUntilRecover :: Actor -> Maybe Int
soundExecUntilRecover sc = case _actorInChannel sc of
  (freq:_) -> Just freq
  _        -> soundExec sc >>= soundExecUntilRecover

soundExec :: Actor -> Maybe Actor
soundExec isc =
  let mins = _actorInstructions isc V.!? _actorIp isc in fmap (incIp . exec' isc) mins
 where
  exec' sc (Send s) = actorSend sc (readValue sc s)
  exec' sc (Receive reg) | readValue sc (RegValue reg) /= 0 = recoverSound sc
                         | otherwise                        = sc

  exec' sc (Set reg val) = execBinOp const sc reg val
  exec' sc (Add reg val) = execBinOp (+) sc reg val
  exec' sc (Mul reg val) = execBinOp (*) sc reg val
  exec' sc (Mod reg val) = execBinOp (flip mod) sc reg val
  exec' sc (Jump val off) | readValue sc val > 0 = modifyActorIp (readValue sc off +) sc
                          | otherwise            = sc

  execBinOp fn sc reg val = updateReg (fn (readValue sc val)) sc reg
  incIp sc | _actorIp sc == _actorIp isc = modifyActorIp (1 +) sc
           | otherwise                   = sc


--------------------------------------------------------------------
-- Actor(aka Part2)
--------------------------------------------------------------------

actor2ExecUntilDeadLock :: Actor -> Actor -> Int
actor2ExecUntilDeadLock ac0 ac1 =
  let mac0' = actorExec ac0
      mac1' = actorExec ac1
  in  case (mac0', mac1') of
        (Just ac0', Just ac1') | not (stuck ac0 ac0' && stuck ac1 ac1') -> length
            (_actorOutChannel ac1')
          + actor2ExecUntilDeadLock (transfterMsgs ac0' ac1') (transfterMsgs ac1' ac0')
        _ -> 0
 where
  stuck old new = _actorIp old == _actorIp new
  transfterMsgs to from =
    to { _actorInChannel = _actorOutChannel from ++ _actorInChannel to, _actorOutChannel = [] }

-- shamelessly copy and pasted and hammered :)
actorExec :: Actor -> Maybe Actor
actorExec ac = let mins = _actorInstructions ac V.!? _actorIp ac in fmap (exec' ac) mins
 where
  exec' sc (Send    s  ) = incIp $ actorSend sc (readValue sc s)
  exec' sc (Receive reg) = case _actorInChannel sc of
    [] -> sc
    corpus ->
      let regUpdated = updateReg (const (last corpus)) sc reg
      in  incIp $ regUpdated { _actorInChannel = init corpus }

  exec' sc (Set reg val) = incIp $ execBinOp const sc reg val
  exec' sc (Add reg val) = incIp $ execBinOp (+) sc reg val
  exec' sc (Mul reg val) = incIp $ execBinOp (*) sc reg val
  exec' sc (Mod reg val) = incIp $ execBinOp (flip mod) sc reg val
  exec' sc (Jump val off) | readValue sc val > 0 = modifyActorIp (readValue sc off +) sc
                          | otherwise            = incIp sc

  execBinOp fn sc reg val = updateReg (fn (readValue sc val)) sc reg
  incIp = modifyActorIp (1 +)


--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------
insParser :: Parser Instruction
insParser =
  sendP
    <|> binop "set" Set
    <|> binop "add" Add
    <|> try (binop "mul" Mul)
    <|> binop "mod" Mod
    <|> receiveP
    <|> jumpP
 where
  binop iden ctor = do
    _ <- symbol iden
    ctor <$> regParser <*> valueParser

  sendP    = Send <$> (symbol "snd" *> valueParser)
  receiveP = Receive <$> (symbol "rcv" *> regParser)
  jumpP    = do
    _ <- symbol "jgz"
    Jump <$> valueParser <*> valueParser

regParser :: Parser Register
regParser = lexeme letterChar

valueParser :: Parser Value
valueParser = (RegValue <$> regParser) <|> (Immediate <$> signedDecimal)
