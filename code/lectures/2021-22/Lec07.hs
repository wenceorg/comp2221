module Lec07 where
import Prelude hiding (Maybe, Nothing, Just, Functor, fmap)

-- Last time, we wrote some linear recursive functions on lists and
-- spotted a common pattern, they all matched

myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ z [] = z
myFold f z (x:xs) = x `f` myFold f z xs

-- For example

length' :: [b] -> Integer
length' = myFold (const (1+)) 0
and' :: [Bool] -> Bool
and' = myFold (&&) True

-- Unsurprisingly, Haskell offers these in the standard library.
-- They are called

-- foldr (for "fold right")
-- foldl (for "fold left")

-- They capture the pattern of linear recursion over a data structure
-- with a right- (respectively left-) associative binary operator.
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' = undefined

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = undefined

-- Easier to think of these non-recursively: replace (:) with `f`, []
-- with z.

-- For example

xs = [1, 2, 3]

sumOfXs = foldr (+) 0 xs

-- [1, 2, 3]
-- == 1 : 2 : 3 : []
-- Replace : with +, and [] with 0
-- 1 + 2 + 3 + 0 == 6

-- There are also foldr1 and foldl1 which operate on non-empty lists

-- This is great, what if I have my own data structures?

-- Generic maps and folds.
-- If we look at the type of map
-- :type map
-- map :: (a -> b) -> [a] -> [b]

-- It explicitly operates on lists, but we already saw some data
-- structures, and we'll see some more now that are also mappable.

-- We've seen a bunch of our own datatypes before, but let's just
-- recapitulate some

-- This is the datatype for a computation that may fail (returning Nothing)
data Maybe a = Nothing | Just a
  deriving (Eq, Show)


-- A binary tree with values at nodes
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)


-- A "Rose tree" with a list of trees at nodes
data RoseTree a = RNode a [RoseTree a]
  deriving (Eq, Show)

-- We already saw a "lifting" function for modifying the value inside
-- a Maybe (in lecture 4)
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

-- Let us suppose we want to transform a BinaryTree
mapBTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBTree f Leaf = Leaf
mapBTree f (Node v left right) = Node (f v) (mapBTree f left) (mapBTree f right)

-- What about a rose tree
mapRTree :: (a -> b) -> RoseTree a -> RoseTree b
mapRTree = undefined


-- This seems annoying, because if we want to swap out a Rose Tree for
-- a Binary Tree, we need to replace all the calls to mapBTree with
-- mapRTree.

-- Enter the Functor type-class
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor
-- This is defined in the standard prelude, but I reproduce it here so
-- you can see it
class Functor f where
  -- A generic map for "containers"
  fmap :: (a -> b) -> f a -> f b
  -- An infix operator to replace all entries in a container with a
  -- default value. e.g. 1 <$ [True, False, True] == [1, 1, 1]
  -- We won't tend to use this, but I show it for completeness
  (<$) :: a -> f b -> f a
  -- Default implementation
  (<$) = fmap . const


-- We now need to provide the fmap instance for lists
instance Functor [] where
  fmap = map

-- Remember we need to write instance declarations for our types
instance Functor Maybe where
  fmap = mapMaybe

instance Functor BinaryTree where
  fmap = mapBTree

instance Functor RoseTree where
  fmap = mapRTree

-- Now we can write generic code.

mappedList :: [Integer]
mappedList = fmap (*2) [1, 2, 3]

mappedMaybe :: Maybe Integer
mappedMaybe = fmap (*2) (Just 4)

mappedBTree :: BinaryTree Integer
mappedBTree = fmap (*2) (Node 4 (Node 6 Leaf Leaf) (Node 2 Leaf (Node 8 Leaf Leaf)))

mappedRTree :: RoseTree Integer
mappedRTree = fmap (*2) (RNode 4 [RNode 6 [], RNode 2 [RNode 8 []]])

-- What about folds, they seem generic too.
-- The pattern of folding over a data structure is captured by the
-- Foldable type class
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:Foldable
-- For which a minimal definition requires us to implement foldr only.

instance Foldable Maybe where
  foldr = undefined

instance Foldable BinaryTree where
  foldr = undefined

instance Foldable RoseTree where
  foldr = undefined


-- Now we can do things like ask
-- What is the length (number of Nodes) of a binary tree?

