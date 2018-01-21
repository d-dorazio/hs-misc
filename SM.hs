#!/usr/bin/env stack
-- stack --resolver lts-10.3 script --package mtl

import Control.Monad.Identity

------------------------------------------------------------
-- Framework
------------------------------------------------------------

-- | 'StateT' is the current state of the machine.
-- * 'End r' if there is nothing more to do and 'r' is the result
-- * 'GoTo f' means that we need to go to state 'f'
-- 'i' is the machine input, 'r' is the final result and 'm' the monad
-- each state is a function that takes an 'i' and returns the new machine
-- state(inside the 'm' monad)
data StateT i r m =
    GoTo (i -> m (StateT i r m))
  | End r

-- | 'State' is just 'StateT' with 'm' = 'Identity'
type State i r = StateT i r Identity

-- | 'goTo' go to the given state
goTo :: Monad m => (i -> m (StateT i r m)) -> m (StateT i r m)
goTo = return . GoTo

-- | 'end' end the machine with the given result
end :: Monad m => r -> m (StateT i r m)
end = return . End

-- | 'step' runs a single step of the machine with the given input
step :: Monad m => StateT i r m -> i -> m (StateT i r m)
step (End  r) _ = return $ End r
step (GoTo f) i = f i

------------------------------------------------------------
-- Example
------------------------------------------------------------
data Binary = O | I deriving (Eq, Show)
newtype Context = Context { runningTime :: Int }
data Result = EngineDead deriving (Eq, Show)

type Machine = Context -> Binary -> IO (StateT Binary Result IO)

idle :: Machine
idle ctx O = do
  putStrLn "sleeping"
  goTo $ idle ctx
idle ctx I = do
  putStrLn "turning on"
  goTo $ running ctx

running :: Machine
running ctx O = do
  putStrLn "turning off"
  goTo $ idle ctx { runningTime = 0 }
running ctx I = if runningTime ctx > 5 then burntOut else running'
 where
  burntOut = do
    putStrLn "the engine burnt out!"
    end EngineDead
  running' = do
    putStrLn "running"
    goTo $ running ctx { runningTime = 1 + runningTime ctx }


------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  initialState <- idle initialCtx O
  finalState   <- foldM step initialState [O, I, I, O, I, I, I, I, I, I, I, I]
  case finalState of
    End  r -> putStr "the machine terminated with " >> print r
    GoTo _ -> putStrLn "the machine did not terminate"
  where initialCtx = Context {runningTime = 0}
