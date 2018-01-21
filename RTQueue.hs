#!/usr/bin/env stack
-- stack --resolver lts-10.3 script

------------------------------------------------------------
-- RTQueue
------------------------------------------------------------

-- realtime queue based on 'Purely Functional Data Structures' by Okasaki
--
-- the main idea is to have 3 lists:
-- _front is a list that represents the front of the queue
-- _rear is the rear of the queue in reverse order
-- _inv is the invariant of the queue
--
-- the invariant is that the front must always be bigger than the rear
-- or put in another way _inv = _front - _rear.
-- this allows amortized O(1) enqueue/dequeue operations.

data RTQueue a = RTQueue
 { _front :: [a]
 , _rear :: [a]
 , _inv :: [a]
 } deriving (Show)

empty :: RTQueue a
empty = RTQueue {_front = [], _rear = [], _inv = []}

enqueue :: a -> RTQueue a -> RTQueue a
enqueue x q = balance q { _rear = x : _rear q }

dequeue :: RTQueue a -> (RTQueue a, Maybe a)
dequeue q = case _front q of
  []     -> (q, Nothing)
  (f:fs) -> (balance q { _front = fs }, Just f)

balance :: RTQueue a -> RTQueue a
balance (RTQueue f r (_:ss)) = RTQueue f r ss
balance (RTQueue f r []    ) = let f' = f ++ reverse r in RTQueue f' [] f'

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = putStrLn "hello world"
