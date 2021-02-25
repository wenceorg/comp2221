{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Exercise06 where
import Data.Foldable -- To make toList available
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- Functor laws
-- fmap id tree = id tree
-- By cases: This is the base of the recursion in the inductive case too
-- fmap id Leaf
-- == Leaf  by definition of fmap
-- == id Leaf by definition of id

-- For the recursive case, we proceed by induction
-- We gain two induction hypotheses, that
-- H1: fmap id l == id l
-- H2: fmap id r == id r
-- fmap id (Node l x r)
-- == Node (fmap id l) (id x) (fmap id r) by definition of fmap
-- == Node (fmap id l) x (fmap id r)      by definition of id
-- == Node l x r                          by application of H1 and H2
-- == id (Node l x r)                     by definition of id

-- Now composition
-- fmap (f . g) tree = fmap f (fmap g tree)

-- Again, we proceed by cases
-- fmap (f . g) Leaf == Leaf  by definition of fmap
-- fmap f (fmap g Leaf) == fmap f Leaf == Leaf by definition of fmap
-- So both sides are equal and we are done

-- For the recursive case again we proceed by induction
-- We gain two induction hypothesis, that
-- H1: fmap (f . g) l == fmap f (fmap g l)
-- H2: fmap (f . g) r == fmap f (fmap g r)
-- fmap (f . g) (Node l x r)
-- == Node (fmap (f . g) l) ((f . g) x) (fmap (f . g) r) by definition of fmap
-- == Node (fmap (f . g) l) (f (g x)) (fmap (f . g) r)   by definition of (.)
-- == Node (fmap f (fmap g l)) (f (g x)) (fmap f (fmap g r)) by application of H1 and H2
-- Now from the other direction
-- fmap f (fmap g (Node l x r))
-- == fmap f (Node (fmap g l) (g x) (fmap g r)) by definition of fmap
-- == Node (fmap f (fmap g l)) (f (g x)) (fmap f (fmap g r)) by definition of fmap
-- Which is what we have so we are done.

-- exercise6-functor.lean does these proofs in lean if you are intereted.

-- This follows what we did in Lecture 8 (Lec08.hs)
instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Node l x r) = x `f` foldr f (foldr f z r) l

-- Conversion of a list to a tree is straightforward
toTree :: [a] -> Tree a
toTree [] = Leaf
-- For the recursive case we peel off the front element and split the
-- remaineder into two parts using splitAt
toTree (x:xs) = Node (toTree ys) x (toTree zs)
  where (ys, zs) = splitAt (length xs `div` 2) xs

depth :: Tree a -> Int
-- Leaves have no depth
depth Leaf = 0
-- Each node adds 1 to the depth, and we max over the two branches
depth (Node l _ r) = 1 + max (depth l) (depth r)

-- Here is the simple way which just recurses and checks the length of
-- the subtrees. This approach is quadratic in the depth, since
-- balanced runs down the tree, and length does as well.
balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node l _ r) =
  balanced l
  && balanced r
  && abs (length l - length r) <= 1

-- We can do better if we realise that we can return the number of
-- leaves we've seen as we come back up the recursion
balanced' :: Tree a -> Bool
balanced' = fst . balanced''
  -- Helper function returns is the tree balanced and the number of
  -- leaves in the subtree
  where balanced'' Leaf = (True, 0)
        -- A node is balanced if both its subtrees are balanced and
        -- the number of leaves differs by at most one.
        -- The number of leaves in this node is 1 + number in left +
        -- number in right
        balanced'' (Node l _ r) = (lb && rb && abs (ln - rn) <= 1,
                                   1 + ln + rn)
          -- Recurse on subtrees and destructure.
          where (lb, ln) = balanced'' l
                (rb, rn) = balanced'' r

-- A tree is symmetric if its left subtree is a mirror image of the
-- right subtree
symmetric :: Tree a -> Bool
symmetric Leaf = True
symmetric (Node l _ r) = mirror l r


-- You may find this useful for symmetric
mirror :: Tree a -> Tree a -> Bool
-- leaves are their own mirror image
mirror Leaf Leaf = True
-- Nodes are mirrors if the subtrees mirror each other
mirror (Node l1 _ r1) (Node l2 _ r2) = mirror l1 r2 && mirror l2 r1
-- otherwise, not a mirror image.
mirror _ _ = False


data Expr = Val Int | Add Expr Expr
  deriving (Eq, Show)

expr = Add (Val 1) (Add (Val 4) (Val 5))

eval :: Expr -> Int
-- Unpack the values
eval (Val x) = x
-- Recurse on Add nodes
eval (Add l r) = eval l + eval r

-- This is straightforward, but quadratic.
collect :: Expr -> [Expr]
collect (Add l r) = collect l ++ collect r
collect v@(Val _) = [v]

-- ASIDE: We can do the same trick we saw when implementing reverse of
-- having a helper function with an accumulator
collectAccumulate :: Expr -> [Expr]
collectAccumulate = go []
  where go vs v@(Val _) = v:vs
        go vs (Add l r) = go (go vs r) l
-- END ASIDE

-- This has the same pattern of recursion as the previous two.
size :: Expr -> Int
size v@(Val _) = 1
size (Add l r) = size l + size r

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- Unwrap the value at apply f.
folde f g (Val v) = f v
-- Recurse on the children and apply g to the result
folde f g (Add l r) = g (folde f g l) (folde f g r)

eval' :: Expr -> Int
-- Applying f to the contents of the  Val node should just return the
-- unwrapped value (which is what id does). We then sum things up.
eval' = folde id (+)

size' :: Expr -> Int
-- Every time we apply f to the contents of Val node we want to return
-- 1, so providing f = const 1 does that.
-- We then sum things up.
size' = folde (const 1) (+)

collect' :: Expr -> [Expr]
-- Applying f to the contents of the Val node should turn it into a
-- singleton list of [Val v], then we concatenate the results with (++)
collect' = folde (\v -> [Val v]) (++)
