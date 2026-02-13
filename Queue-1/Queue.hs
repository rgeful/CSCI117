-- Simple Queue Interface

module Queue (Queue(..)) where

class Queue q where
  mtq  :: q a                    -- empty queue
  addq :: a -> q a -> q a        -- add element to back of queue
  remq :: q a -> Maybe (a, q a)  -- remove element from front of queue
