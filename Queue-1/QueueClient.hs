module QueueClient where

import Queue   -- Interface
import Queue1  -- Queue1 implementation
import Queue2  -- Queue2 implementation

-- A data type representing operations that can be performed on a Queue:
-- * The op `A x` represents adding `x` to a queue
-- * The op `R` represents removing an element from a queue
data Qop a = A a | R deriving Show

-- Perform a series of operations on a given queue ("accumulating" the results)
-- and return the list of all elements removed, in the order they were removed.
-- Give an error if a remq is attempted on an empty queue.
perf :: Queue q => [Qop a] -> q a -> [a]
perf [] _ = []
perf (A x : ops) q = perf ops (addq x q)
perf (R : ops) q = case remq q of
  Nothing      -> error "remq on empty queue"
  Just (x, q') -> x : perf ops q'

{-
Your tests here. For each test, show that both implementations produce the
same result. For example, start with mtq :: Queue1 Int and mtq :: Queue2 Int

perf [A 1, A 2, A 3, R, R] (mtq :: Queue1 Int) => [1,2]
perf [A 1, A 2, A 3, R, R] (mtq :: Queue2 Int) => [1,2]

perf [A 1, R, A 2, R, A 3, R] (mtq :: Queue1 Int) => [1,2,3]
perf [A 1, R, A 2, R, A 3, R] (mtq :: Queue2 Int) => [1,2,3]

perf [] (mtq :: Queue1 Int) => []
perf [] (mtq :: Queue2 Int) => []
-}
