module Queue1 (Queue1) where

import Queue

---- Implementation -----------

{- In this implementation, a queue is represented as an ordinary list.
The "front" of the queue is the head of the list, and the "back" of
the queue is the end of the list. Elements are removed from the front,
and new elements are added to the back.
-}

data Queue1 a = Queue1 [a]

instance Queue Queue1 where
  mtq = Queue1 []
  addq x (Queue1 xs) = Queue1 (xs ++ [x])
  remq (Queue1 [])     = Nothing
  remq (Queue1 (x:xs)) = Just (x, Queue1 xs)

{-
Your tests here (use "deriving Show"). Start with mtq :: Queue1 Int

mtq :: Queue1 Int                         => Queue1 []
addq 1 mtq :: Queue1 Int                  => Queue1 [1]
addq 3 (addq 2 (addq 1 mtq)) :: Queue1 Int => Queue1 [1,2,3]
remq (mtq :: Queue1 Int)                  => Nothing
remq (addq 1 (mtq :: Queue1 Int))         => Just (1, Queue1 [])
remq (addq 3 (addq 2 (addq 1 mtq)) :: Queue1 Int) => Just (1, Queue1 [2,3])
-}
