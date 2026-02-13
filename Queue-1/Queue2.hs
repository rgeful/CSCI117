module Queue2 (Queue2) where

import Queue

--- Implementation -----------

{- In this implementation, a queue is represented as a pair of lists,
a "front part" and a reversed "back part". Elements are removed from
the head of the front part, and new elements are added to the front of
the back part. If the front part becomes empty, the back part is reversed
and becomes the new front part, leaving the back part empty.
-}

data Queue2 a = Queue2 [a] [a]

instance Queue Queue2 where
  mtq = Queue2 [] []
  addq x (Queue2 front back) = Queue2 front (x:back)
  remq (Queue2 [] [])   = Nothing
  remq (Queue2 [] back) = remq (Queue2 (reverse back) [])
  remq (Queue2 (x:front) back) = Just (x, Queue2 front back)

{-
Your tests here (use "deriving Show"). Start with mtq :: Queue2 Int

mtq :: Queue2 Int                          => Queue2 [] []
addq 1 mtq :: Queue2 Int                   => Queue2 [] [1]
addq 3 (addq 2 (addq 1 mtq)) :: Queue2 Int => Queue2 [] [3,2,1]
remq (mtq :: Queue2 Int)                   => Nothing
remq (addq 1 (mtq :: Queue2 Int))          => Just (1, Queue2 [] [])
remq (addq 3 (addq 2 (addq 1 mtq)) :: Queue2 Int) => Just (1, Queue2 [2,3] [])
-}
