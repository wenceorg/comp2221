-- With these magic language extensions, GHC can derive Functor and
-- Foldable instances for us.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Lec08 where
import Prelude hiding (Maybe, Nothing, Just)
import Debug.Trace
import Data.Foldable
-- A binary tree
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show, Functor)

-- A "Rose tree" with a list of trees at nodes
-- Also called a "multi-way tree"
-- I realised afterwards that we don't actually need a separate RLeaf,
-- because empty list will do fine
data RoseTree a = RNode a [RoseTree a]
  deriving (Eq, Show, Functor)

-- We didn't do this and will pick it up next time.
-- What about folds? They seem generic too.

-- :info foldr
-- :info Foldable

-- Once again we're just implementing this standard type so that we
-- can show how to implement a Foldable instance for it. Notice this
-- time we ask GHC to derive the Functor instance.
-- If we wanted to derive the Foldable instance too, we would add
-- Foldable to this list of deriving type classes.
data Maybe a = Nothing | Just a
  deriving (Eq, Show, Functor)

-- Here we demonstrate that not every "container-like" type is
-- functorial. This is a container for all unary functions from a to
-- a. To provide a Functor instance, we would have to provide a
-- witness fmap that obeys the two Functor laws
data Fn a = MkFn (a -> a)

-- It turns out we can't do this. We can produce something that
-- typechecks by choosing
fmapFn f (MkFn g) = MkFn id
-- But this doesn't obey the requirement that fmapFn id f == id f == f

-- Now let's look at some Foldable instances. The idea here is that we
-- have a function for combining an a and a b :: a -> b -> b and want
-- to convert a container of as (here a Maybe a) into a single b.
-- We also have an initial z :: b that we can use to deliver a b when
-- our container is empty.
instance Foldable Maybe where
  -- In the empty case, the only thing of type b we have to hand is z,
  -- so let's just hand that back.
  foldr f z Nothing = z
  -- Otherwise we have a single x :: a, so let's combine it with z ::
  -- b to produce a new value x `f` z  :: b
  foldr f z (Just x) = x `f` z

-- The game is similar for the binary tree case.
instance Foldable BinaryTree where
  -- Again, we have an empty tree, so we can only deliver z.
  foldr _ z Leaf = z
  -- This time we're going to combine x with the result of recursion.
  -- We need to transform both the left and right trees
  -- We choose to recurse down the right branch to generate a new
  -- initial value, and use that as the base of the recursion down the
  -- left branch.
  -- This results in our fold visiting the leaves of the tree
  -- left-to-right.
  foldr f z (Node x left right) = x `f` foldr f (foldr f z right) left

instance Foldable RoseTree where
  -- For the rose tree, we have an x :: a in the leaf case, so use it.
  foldr f z (RNode x []) = x `f` z
  -- Again we must recurse.
  -- We need to fold over each tree in the list of trees so we combine
  -- x with the result of doing that. For each tree, we need to bundle
  -- it up into a single value of type b. We do that by folding over
  -- the tree (so we fold g over a list of trees where g is itself a
  -- fold over the tree (using f).
  foldr f z (RNode x trees) = x `f` foldr g z trees
    -- Note that this could be written succintly as
    -- g = flip (foldr f)
    -- at which point we could write this definition on one line as
    -- x `f` foldr (flip (foldr f)) z trees
    where g x acc = foldr f acc x


-- Some lazy evaluation
-- F_1 = 1
-- F_2 = 1
-- F_n = F_{n-1} + F_{n-2}
-- Haskell postpones creating the result of the list until such time
-- as we look at it. When we come to peel off the next entry we have
-- just enough information to compute it.
fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

-- This kind of thing is fine too
inf :: Integer
inf = 1 + inf

-- As long as we don't look, everything is fine
-- If we try and do snd (1, inf) things will not go well for us.
y = fst (1, inf)
