module TuringMachine (
    solve
  , State
  , Register
  , TuringMachine (..)
  , Tape (..)
  , TapeMove (..)
  , Transition (..)
  , mkTape
  , tapeToList
  , moveTape
  , runUntilCheckpoint
  , runTuringStep
  , turingMachineParser
) where

import Control.Applicative ((<|>))

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

import Text.Megaparsec (between, some)
import Text.Megaparsec.Char (letterChar)

import Lib (Part(..), Parser, parseOrPrettyErr, symbol, decimal)

solve :: Part -> String -> String
solve _ = parseOrPrettyErr turingMachineParser $ \tm ->
  show . length . filter (== 1) . tapeToList . _machineTape . fromJust . runUntilCheckpoint $ tm


type State = Char
type Register = Int

data TuringMachine = TuringMachine
  { _machineState :: State
  , _machineTape :: Tape
  , _machineTransitions :: M.Map (State, Register) Transition
  , _machineChecksumSteps :: Int
  } deriving (Show)

data Tape = Tape
  { _tapeLeftRegs :: [Register] -- reverse order
  , _tapeCurReg :: Register
  , _tapeRightRegs :: [Register]
  } deriving (Show)

data TapeMove = Prev | Next deriving (Show)

mkTape :: Tape
mkTape = Tape {_tapeLeftRegs = [], _tapeRightRegs = [], _tapeCurReg = 0}

tapeToList :: Tape -> [Register]
tapeToList (Tape left cur right) = reverse left ++ [cur] ++ right

moveTape :: TapeMove -> Tape -> Tape
moveTape Prev (Tape []       cur right    ) = Tape [] 0 (cur : right)
moveTape Prev (Tape (l:left) cur right    ) = Tape left l (cur : right)
moveTape Next (Tape left     cur []       ) = Tape (cur : left) 0 []
moveTape Next (Tape left     cur (r:right)) = Tape (cur : left) r right

data Transition = Transition
  { _transitionNewReg   :: Register
  , _transitionTapeMove :: TapeMove
  , _transitionNewState :: State
  } deriving (Show)


runUntilCheckpoint :: TuringMachine -> Maybe TuringMachine
runUntilCheckpoint tm | _machineChecksumSteps tm <= 0 = Just tm
                      | otherwise                     = runTuringStep tm >>= runUntilCheckpoint

runTuringStep :: TuringMachine -> Maybe TuringMachine
runTuringStep (TuringMachine state tape trans steps) = fmap
  run
  (trans M.!? (state, _tapeCurReg tape))
 where
  run t =
    let tape'  = moveTape (_transitionTapeMove t) tape { _tapeCurReg = _transitionNewReg t }
        state' = _transitionNewState t
        steps' = steps - 1
    in  TuringMachine state' tape' trans steps'

--------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------

turingMachineParser :: Parser TuringMachine
turingMachineParser = do
  initialState <- beginP
  steps        <- stepsP
  transitions  <- some transitionsP
  return $ TuringMachine
    { _machineState         = initialState
    , _machineTape          = mkTape
    , _machineTransitions   = M.fromList . concat $ transitions
    , _machineChecksumSteps = steps
    }
 where
  beginP       = between (symbol "Begin in state") dot letterChar
  stepsP       = between (symbol "Perform a diagnostic checksum after") (symbol "steps.") decimal

  transitionsP = do
    st    <- between (symbol "In state") colon letterChar
    trans <- some ((,) <$> currentValueP <*> transitionP)
    return . map (\(r, t) -> ((st, r), t)) $ trans

  currentValueP = between (symbol "If the current value is") colon decimal

transitionP :: Parser Transition
transitionP = do
  newVal   <- between (symbol "- Write the value") dot decimal
  move     <- between (symbol "- Move one slot to the") dot moveP
  newState <- between (symbol "- Continue with state") dot letterChar

  return $ Transition
    { _transitionNewReg   = newVal
    , _transitionTapeMove = move
    , _transitionNewState = newState
    }

moveP :: Parser TapeMove
moveP = (const Prev <$> symbol "left") <|> (const Next <$> symbol "right")

dot :: Parser String
dot = symbol "."

colon :: Parser String
colon = symbol ":"
