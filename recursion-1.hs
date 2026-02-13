---- Part 1: Merge Sort ----------------

-- Deal a list into two (almost) equal-sized lists by alternating elements
-- For example, deal [1,2,3,4,5,6,7] = ([1,3,5,7], [2,4,6])
-- and          deal   [2,3,4,5,6,7] = ([2,4,6], [3,5,7])
-- Hint: notice what's happening between the answers to deal [2..7] and
-- deal (1:[2..7]) above to get an idea of how to approach the recursion
{- HLINT ignore "Redundant lambda" -}
deal :: [a] -> ([a],[a])
deal [] = ([],[])
deal (x:xs) = let (ys,zs) = deal xs
              in (x:zs, ys)

-- Now implement merge and mergesort (ms), and test with some
-- scrambled lists to gain confidence that your code is correct
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | x > y  = y : merge (x:xs) ys

ms :: Ord a => [a] -> [a]
ms [] = []
ms [x] = [x]
ms xs = let (ys, zs) = deal xs 
        in merge (ms ys) (ms zs)   -- general case: deal, recursive call, merge

{-
Your tests here
deal [1,2,3,4,5,6,7]    => ([1,3,5,7],[2,4,6])
merge [1,3,5,7] [2,4,6] => [1,2,3,4,5,6,7]
ms [4,1,7,3,8,5,2,6]    => [1,2,3,4,5,6,7,8]
-}


---- Part 2: Backward Lists ----------------

-- Back Lists: Lists where elements are added to the back ("snoc" == rev "cons")
-- For example, the list [1,2,3] is represented as Snoc (Snoc (Snoc Nil 1) 2) 3
data BList a = Nil | Snoc (BList a) a deriving (Show,Eq)

-- Add an element to the beginning of a BList, like (:) does
cons :: a -> BList a -> BList a
cons x Nil = Snoc Nil x
cons x (Snoc bs y) = Snoc (cons x bs) y

-- Convert a usual list into a BList (hint: use cons in the recursive case)
toBList :: [a] -> BList a
toBList [] = Nil
toBList (x:xs) = cons x (toBList xs)

-- Add an element to the end of an ordinary list, like Snoc does
snoc :: [a] -> a -> [a]
snoc [] x = [x]
snoc (y:ys) x = y : snoc ys x
          

-- Convert a BList into an ordinary list (hint: use snoc in the recursive case)
fromBList :: BList a -> [a]
fromBList Nil = []
fromBList (Snoc bs x) = snoc (fromBList bs) x


-- Both toBList and fromBList above are O(n^2) operations. Reimplement them
-- using iterative helper functions (locally defined using a 'where' clause)
-- with accumulators to make them O(n)
toBList' :: [a] -> BList a
toBList' xs = go xs Nil where
  go [] acc = acc
  go (x:xs) acc = go xs (Snoc acc x)


fromBList' :: BList a -> [a]
fromBList' bl = go bl [] where
  go Nil acc = acc
  go (Snoc bs x) acc = go bs (x:acc)

            

{-
Your tests here

-- cons
cons 0 Nil                              => Snoc Nil 0
cons 0 (Snoc (Snoc (Snoc Nil 1) 2) 3)  => Snoc (Snoc (Snoc (Snoc Nil 0) 1) 2) 3

-- toBList
toBList []      => Nil
toBList [1,2,3] => Snoc (Snoc (Snoc Nil 1) 2) 3

-- snoc
snoc [] 1       => [1]
snoc [1,2,3] 4  => [1,2,3,4]

-- fromBList
fromBList Nil                             => []
fromBList (Snoc (Snoc (Snoc Nil 1) 2) 3) => [1,2,3]

-- toBList' (same results as toBList, but O(n))
toBList' []      => Nil
toBList' [1,2,3] => Snoc (Snoc (Snoc Nil 1) 2) 3

-- fromBList' (same results as fromBList, but O(n))
fromBList' Nil                             => []
fromBList' (Snoc (Snoc (Snoc Nil 1) 2) 3) => [1,2,3]
-}


---- Part 3: Binary Trees ----------------

-- Binary trees without explicit leaves
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Count number of Empty's in the tree
num_empties :: Tree a -> Int
num_empties Empty = 1
num_empties (Node _ left right) = num_empties left + num_empties right

-- Count number of Node's in the tree
num_nodes :: Tree a -> Int
num_nodes Empty = 0
num_nodes (Node _ left right) = 1 + num_nodes left + num_nodes right

-- Insert a new node in the leftmost spot in the tree
insert_left :: a -> Tree a -> Tree a
insert_left x Empty = Node x Empty Empty
insert_left x (Node n left right) = Node n (insert_left x left) right

-- Insert a new node in the rightmost spot in the tree
insert_right :: a -> Tree a -> Tree a
insert_right x Empty = Node x Empty Empty
insert_right x (Node n left right) = Node n left (insert_right x right)

-- Add up all the node values in a tree of numbers
sum_nodes :: Num a => Tree a -> a
sum_nodes Empty = 0
sum_nodes (Node n left right) = n + sum_nodes left + sum_nodes right

-- Produce a list of the node values in the tree via an inorder traversal
-- Feel free to use concatenation (++)
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node n left right) = inorder left ++ [n] ++ inorder right

-- Even tree functions that do multiple recursive calls can be rewritten
-- iteratively using lists of trees and an accumulator. For example,
sum_nodes' :: Num a => Tree a -> a
sum_nodes' t = sum_nodes_it [t] 0 where
  sum_nodes_it :: Num a => [Tree a] -> a -> a
  sum_nodes_it [] a = a
  sum_nodes_it (Empty:ts) a = sum_nodes_it ts a
  sum_nodes_it (Node n t1 t2:ts) a = sum_nodes_it (t1:t2:ts) (n+a)

-- Use the same technique to convert num_empties and num_nodes into
--  iterative functions with accumulators

num_empties' :: Tree a -> Int
num_empties' t = go [t] 0 where
  go [] acc = acc
  go (Empty:ts) acc = go ts (acc + 1)
  go (Node _ t1 t2:ts) acc = go (t1:t2:ts) acc

num_nodes' :: Tree a -> Int
num_nodes' t = go [t] 0 where
  go [] acc = acc
  go (Empty:ts) acc = go ts acc
  go (Node _ t1 t2:ts) acc = go (t1:t2:ts) (acc + 1)

{-
Your tests here

-- t = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
--       5
--      / \
--     3   7
--    / \ / \
--   E  E E  E

-- num_empties
num_empties Empty                                           => 1
num_empties (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) => 4

-- num_nodes
num_nodes Empty                                               => 0
num_nodes (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))  => 3

-- insert_left
insert_left 1 Empty            => Node 1 Empty Empty
insert_left 1 (Node 5 Empty Empty) => Node 5 (Node 1 Empty Empty) Empty

-- insert_right
insert_right 1 Empty               => Node 1 Empty Empty
insert_right 1 (Node 5 Empty Empty) => Node 5 Empty (Node 1 Empty Empty)

-- sum_nodes
sum_nodes Empty                                               => 0
sum_nodes (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))  => 15

-- inorder
inorder Empty                                               => []
inorder (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))  => [3,5,7]

-- num_empties' (same results as num_empties, but iterative)
num_empties' Empty                                              => 1
num_empties' (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)) => 4

-- num_nodes' (same results as num_nodes, but iterative)
num_nodes' Empty                                               => 0
num_nodes' (Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty))  => 3
-}


---- Part 4: Higher-order functions ----------------

-- The functions map, all, any, filter, dropWhile, takeWhile, and break
-- from the Prelude are all higher-order functions. Reimplement them here
-- as list recursions. break should process each element of the list at
-- most once. All functions should produce the same output as the originals.
-- I've implemented the first one for you (in two ways), as an illustration.

my_map :: (a -> b) -> [a] -> [b]
my_map f [] = []
my_map f (x:xs) = f x : my_map f xs

{- Alternatively, since f doesn't change during the recursion, we could
   also write it this way, since f is in the scope of the `go` function:

   my_map f xs = go xs where
     go [] = []
     go (x:xs) = f x : go xs
-}

my_all :: (a -> Bool) -> [a] -> Bool
my_all _ [] = True
my_all f (x:xs) = f x && my_all f xs

my_any :: (a -> Bool) -> [a] -> Bool
my_any _ [] = False
my_any f (x:xs) = f x || my_any f xs

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f (x:xs)
  | f x       = x : my_filter f xs
  | otherwise = my_filter f xs

my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile _ [] = []
my_dropWhile f (x:xs)
  | f x       = my_dropWhile f xs
  | otherwise = x:xs

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile _ [] = []
my_takeWhile f (x:xs)
  | f x       = x : my_takeWhile f xs
  | otherwise = []

my_break :: (a -> Bool) -> [a] -> ([a], [a])
my_break _ [] = ([], [])
my_break f (x:xs)
  | f x       = ([], x:xs)
  | otherwise = let (ys, zs) = my_break f xs in (x:ys, zs)

-- Implement the Prelude functions and, or, concat using foldr

my_and :: [Bool] -> Bool
my_and = foldr (&&) True

my_or :: [Bool] -> Bool
my_or = foldr (||) False

my_concat :: [[a]] -> [a]
my_concat = foldr (++) []

-- Implement the Prelude functions sum, product, reverse using foldl

my_sum :: Num a => [a] -> a
my_sum = foldl (+) 0

my_product :: Num a => [a] -> a
my_product = foldl (*) 1

my_reverse :: [a] -> [a]
my_reverse = foldl (flip (:)) []

{-
Your tests here

-- my_all
my_all even [2,4,6]     => True
my_all even [2,3,6]     => False
my_all even []          => True

-- my_any
my_any even [1,3,5]     => False
my_any even [1,2,3]     => True
my_any even []          => False

-- my_filter
my_filter even [1,2,3,4,5]  => [2,4]
my_filter even []            => []

-- my_dropWhile
my_dropWhile even [2,4,1,2]  => [1,2]
my_dropWhile even [1,2,3]    => [1,2,3]

-- my_takeWhile
my_takeWhile even [2,4,1,2]  => [2,4]
my_takeWhile even [1,2,3]    => []

-- my_break
my_break even [1,2,3,4]  => ([1],[2,3,4])
my_break even [2,3,4]    => ([],[2,3,4])
my_break even [1,3,5]    => ([1,3,5],[])

-- my_and
my_and [True, True, True]   => True
my_and [True, False, True]  => False
my_and []                   => True

-- my_or
my_or [False, False, False]  => False
my_or [False, True, False]   => True
my_or []                     => False

-- my_concat
my_concat [[1,2],[3,4],[5]]  => [1,2,3,4,5]
my_concat []                 => []

-- my_sum
my_sum [1,2,3,4,5]  => 15
my_sum []           => 0

-- my_product
my_product [1,2,3,4,5]  => 120
my_product []            => 1

-- my_reverse
my_reverse [1,2,3,4,5]  => [5,4,3,2,1]
my_reverse []            => []

-}
