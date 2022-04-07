module Lec08 where
import Prelude hiding (Maybe, Just, Nothing)
-- This is the datatype for a computation that may fail (returning Nothing)
data Maybe a = Nothing | Just a
  deriving (Eq, Show)

-- A binary tree with values at nodes
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)


-- A "Rose tree" with a list of trees at nodes
data RoseTree a = RNode a [RoseTree a]
  deriving (Eq, Show)

-- Remember we need to write instance declarations for our types
instance Functor Maybe where
  -- We saw this one already
  fmap _ Nothing = Nothing
  fmap fab (Just a) = Just (fab a)

instance Functor BinaryTree where
  -- The structural recursion here is very similar
  fmap fab Leaf = Leaf
  fmap fab (Node a left right) = Node (fab a) (fmap fab left) (fmap fab right)

-- Needs to obey fmap id == id
-- and fmap f (fmap g tree) == fmap (f . g) tree

instance Functor RoseTree where
  -- This one is quite cute.
  fmap fab (RNode a rts) = RNode (fab a) [fmap fab rt | rt <- rts]

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
  -- We can think of Maybe as equivalent to a singleton list
  -- And "copy" the list definition
  -- foldr :: Foldable f => (a -> b -> b) -> b -> f a -> b
  -- In the empty case, the only thing of type b we have to hand is z,
  -- so let's just hand that back.
  foldr _ z Nothing = z
  -- Otherwise we have a single x :: a, so let's combine it with z ::
  -- b to produce a new value x `f` z  :: b
  foldr f z (Just x) = x `f` z

instance Foldable BinaryTree where
  -- Again, we have an empty tree, so we can only deliver z.
  foldr _ z Leaf = z
  -- The tricky thing here is to recurse down in correct order to get
  -- to the "end".
  -- We need to transform both the left and the right trees. Since
  -- we're implementing foldr, we recurse down the right branch to
  -- generate a new initial value, and use that as the "z" for the
  -- recursion down the left branch.
  -- This results in our fold visiting the leaves of the tree left to
  -- right.
  -- That is, running foldr (+) 0 over the pictorial tree shown here,
  -- we collapse things as parenthesised (which is correct for a
  -- right-associative operator)
  -- foldr (+) 0
  --   1                  1                    1 + (2 + (3 + (5 + (6 + 0))))
  --  / \                / \
  -- 2   3        =>    2   3 + (5 + (6+0)) =>
  --    / \
  --   5   6
  foldr f z (Node x left right) = x `f` foldr f newz left
    where newz = foldr f z right

instance Foldable RoseTree where
  -- Here we don't have to case split anything, but we destructure the
  -- RNode into a value and list of RoseTrees (that may be empty)
  -- What's going on here?
  -- We need to fold over each tree in the list of child trees so we
  -- combine x with the result of doing that.
  -- For each tree, we need to bundle it up into a single value of
  -- type b (which is what `foldr f z tree` does). So we end up
  -- folding g over a list of trees where g is itself a fold over the
  -- tree using f.
  foldr f z (RNode x xs) = x `f` foldr g z xs
    -- Note that this definition is equivalent to `g = flip (foldr f)`
    -- at which point we could have written everything on one line
    -- easily as the slightly incomprehensible
    -- x `f` foldr (flip (foldr f)) z trees
    where g rt z' = foldr f z' rt
  
-- Now we can do things like ask
-- What is the length (number of Nodes) of a binary tree?

-- Lazy evaluation
data Nat = Zero | Succ Nat
  deriving (Eq, Show, Ord)


zero :: Nat
zero = Zero
one :: Nat
one = Succ zero
inf :: Nat
inf = Succ inf

-- F_0 = 1
-- F_1 = 1
-- F_n = F_{n-1} + F_{n-2}
-- Haskell postpones creating the full list until such time as we look
-- at it. When we peel off the next entry we have just enough
-- information to compute it.
-- This list is already in weak head normal form (WHNF), so until we
-- look at it, no computation occurs.
fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)
