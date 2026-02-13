---- Practice Problems ----------------
-- Based on recursion-1.hs and Queue lab
-- Covers likely exam question patterns


---- Section 1: New recursive list functions ----------------

-- Return the length of a list
my_length :: [a] -> Int
my_length [] = 0
my_length (_:xs) = 1 + my_length xs

-- Check if an element is in a list
my_elem :: Eq a => a -> [a] -> Bool
my_elem _ [] = False
my_elem x (y:ys) = x == y || my_elem x ys

-- Zip two lists into a list of pairs (stop at the shorter one)
my_zip :: [a] -> [b] -> [(a, b)]
my_zip [] _ = []
my_zip _ [] = []
my_zip (x:xs) (y:ys) = (x, y) : my_zip xs ys

-- Remove duplicate elements (keep first occurrence)
my_nub :: Eq a => [a] -> [a]
my_nub [] = []
my_nub (x:xs) = x : my_nub (my_filter (\y -> y /= x) xs)
  where
    my_filter _ [] = []
    my_filter f (z:zs)
      | f z       = z : my_filter f zs
      | otherwise = my_filter f zs

{-
Tests:
my_length []         => 0
my_length [1,2,3]    => 3

my_elem 3 [1,2,3]    => True
my_elem 4 [1,2,3]    => False

my_zip [1,2,3] "abc" => [(1,'a'),(2,'b'),(3,'c')]
my_zip [1,2] "abcd"  => [(1,'a'),(2,'b')]

my_nub [1,2,1,3,2]   => [1,2,3]
my_nub []             => []
-}


---- Section 2: New BList and Tree functions ----------------

-- BList from recursion-1
data BList a = Nil | Snoc (BList a) a deriving (Show, Eq)

-- Apply a function to every element of a BList
mapBList :: (a -> b) -> BList a -> BList b
mapBList _ Nil = Nil
mapBList f (Snoc bs x) = Snoc (mapBList f bs) (f x)

-- Count elements in a BList
lengthBList :: BList a -> Int
lengthBList Nil = 0
lengthBList (Snoc bs _) = 1 + lengthBList bs

-- Tree from recursion-1
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Depth (longest path from root to Empty)
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- Mirror a tree (swap left and right at every node)
mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Node n left right) = Node n (mirror right) (mirror left)

-- Apply a function to every node value
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node n left right) = Node (f n) (mapTree f left) (mapTree f right)

{-
-- t = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)

Tests:
mapBList (+1) (Snoc (Snoc Nil 1) 2)  => Snoc (Snoc Nil 2) 3
lengthBList (Snoc (Snoc (Snoc Nil 1) 2) 3) => 3
lengthBList Nil => 0

depth Empty                                              => 0
depth (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) => 2

mirror (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
  => Node 5 (Node 7 Empty Empty) (Node 3 Empty Empty)

mapTree (*2) (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))
  => Node 10 (Node 6 Empty Empty) (Node 14 Empty Empty)
-}


---- Section 3: O(n^2) -> O(n) with accumulator ----------------

-- Naive O(n^2): depth using reverse-and-prepend style (example of bad pattern)
-- Here we convert my_length to an iterative O(n) version

my_length' :: [a] -> Int
my_length' xs = go xs 0 where
  go [] acc = acc
  go (_:xs) acc = go xs (acc + 1)

-- Iterative mapBList using a worklist approach
-- (analogous to num_empties' / sum_nodes' from the lab)
sumBList :: Num a => BList a -> a
sumBList bl = go bl 0 where
  go Nil acc = acc
  go (Snoc bs x) acc = go bs (acc + x)

-- Iterative depth using a worklist of (tree, currentDepth) pairs
depth' :: Tree a -> Int
depth' t = go [(t, 0)] 0 where
  go [] maxD = maxD
  go ((Empty, d) : rest) maxD = go rest (max d maxD)
  go ((Node _ l r, d) : rest) maxD = go ((l, d+1) : (r, d+1) : rest) maxD

{-
Tests:
my_length' [1,2,3,4,5]  => 5
my_length' []            => 0

sumBList (Snoc (Snoc (Snoc Nil 1) 2) 3) => 6
sumBList Nil                             => 0

depth' Empty                                              => 0
depth' (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) => 2
-}


---- Section 4: foldr and foldl ----------------

-- Find the maximum element using foldr (list must be non-empty)
my_maximum :: Ord a => [a] -> a
my_maximum [x]    = x
my_maximum (x:xs) = max x (my_maximum xs)
-- equivalently: my_maximum (x:xs) = foldr max x xs

-- Count elements satisfying a predicate using foldr
my_count :: (a -> Bool) -> [a] -> Int
my_count f = foldr (\x acc -> if f x then acc + 1 else acc) 0

-- Flatten a list of lists using foldr
my_flatten :: [[a]] -> [a]
my_flatten = foldr (++) []

-- Check if a list is sorted using foldr-style recursion
my_sorted :: Ord a => [a] -> Bool
my_sorted [] = True
my_sorted [_] = True
my_sorted (x:y:rest) = x <= y && my_sorted (y:rest)

{-
Tests:
my_maximum [3,1,4,1,5,9]   => 9
my_maximum [1]              => 1

my_count even [1,2,3,4,5]  => 2
my_count even []            => 0

my_flatten [[1,2],[3],[4,5]] => [1,2,3,4,5]
my_flatten []                => []

my_sorted [1,2,3,4]   => True
my_sorted [1,3,2,4]   => False
my_sorted []          => True
-}


---- Section 5: New Queue operation ----------------

-- Queue interface (copied from Queue.hs for self-containment)
class Queue q where
  mtq  :: q a
  addq :: a -> q a -> q a
  remq :: q a -> Maybe (a, q a)

-- Queue1: simple list
data Queue1 a = Queue1 [a] deriving Show

instance Queue Queue1 where
  mtq = Queue1 []
  addq x (Queue1 xs) = Queue1 (xs ++ [x])
  remq (Queue1 [])     = Nothing
  remq (Queue1 (x:xs)) = Just (x, Queue1 xs)

-- Queue2: two-list amortised
data Queue2 a = Queue2 [a] [a] deriving Show

instance Queue Queue2 where
  mtq = Queue2 [] []
  addq x (Queue2 front back) = Queue2 front (x:back)
  remq (Queue2 [] [])   = Nothing
  remq (Queue2 [] back) = remq (Queue2 (reverse back) [])
  remq (Queue2 (x:front) back) = Just (x, Queue2 front back)

-- Peek at the front element without removing it
peekq :: Queue q => q a -> Maybe a
peekq q = case remq q of
  Nothing     -> Nothing
  Just (x, _) -> Just x

-- Get the size of a queue by draining it
sizeq :: Queue q => q a -> Int
sizeq q = case remq q of
  Nothing     -> 0
  Just (_, q') -> 1 + sizeq q'

{-
Tests (Queue1):
peekq (mtq :: Queue1 Int)                          => Nothing
peekq (addq 1 (addq 2 (mtq :: Queue1 Int)))        => Just 1
sizeq (mtq :: Queue1 Int)                          => 0
sizeq (addq 3 (addq 2 (addq 1 (mtq :: Queue1 Int)))) => 3

Tests (Queue2):
peekq (mtq :: Queue2 Int)                          => Nothing
peekq (addq 1 (addq 2 (mtq :: Queue2 Int)))        => Just 1
sizeq (addq 3 (addq 2 (addq 1 (mtq :: Queue2 Int)))) => 3
-}
