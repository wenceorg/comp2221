module Lec05 where
import Prelude hiding (drop, concat, product, sum, maximum)


-- Let's finish off our manipulation of our List type
data List a = Nil | Cons a (List a)
  deriving Show

-- Converting builtin lists to our type
-- To avoid tediously writing out large lists by hand, we can convert
-- builtin lists to our data type by just translating [] to Nil and
-- (:) to Cons.
fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

-- Length of our list type
-- This is the natural definition
-- Ideally we would want to implement `length` instead, but we
-- postpone that until we talk about the Foldable type class in a few
-- lectures time.
len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

-- This time we will write the Eq instance declaration
-- I didn't do this in the lecture (I skipped the comment) but we can
-- do it here. Remember that we introduce an instance of a type class
-- with the `instance` keyword.
-- Here we want to be able to provide equality on (List a) as long as
-- `a` provides equality
-- Generally we would use the `deriving Eq` version that Haskell
-- offers, but sometimes we might have a more efficient implementation
-- in mind than the default which will just do structural recursion.
instance Eq a => Eq (List a) where
  -- now we need to provide the implementation of (at least) one of
  -- (==) or (/=), we can provide both.
  -- Here we'll just provide (==)
  -- First we do the recursive case where the list contains at least
  -- one element.
  -- Because we stated that we have equality on `a` (in the instance
  -- declaration) we can compare the head of the two lists, and then
  -- recurse on the tail. So two lists are equal to one another if
  -- their heads are equal, and their tails are equal.
  (Cons x xs) == (Cons y ys) = x == y && (xs == ys)
  -- The empty list is equal to itself
  Nil         == Nil         = True
  -- All other combinations are false.
  _           == _           = False

-- We had already written appending one list to another
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)


-- Now let's reverse the list in two different ways
-- First a simple, inefficient, way
-- We need to move the head of the list to the end of the reversed tail.
rev :: List a -> List a
-- Reversing an empty list returns the empty list
rev Nil = Nil
-- append the reverse of tail onto the singleton list containing the head.
rev (Cons x xs) = append (rev xs) (Cons x Nil)

-- We then saw that this was monumentally slow. Running GHCi with :set
-- +s, it prints the time to evaluate an expression, and also the
-- amount of memory allocation. Reversing a 10000-element List took
-- over 7 seconds and allocated nearly 9GB of data.
--
-- The reason for this is that `rev` walks down to the end of the list
-- every time, and `append` does as well, so we actually have an
-- algorithm that is quadratic in the length of our input list.
-- reversing a linked list is something we can do in linear time.
-- Let's see how to do that now.

{-

In a language where we can mutate variables, we would probably write
the reversal of a list like so.

First, initialise the result value to an empty list

ys = Nil

Then loop over the elements of the input list, and cons the value onto
the front of the result

for x in xs:
  ys = Cons x ys

At the end of this loop, ys contains the reverse of xs.

Imagine that xs is (Cons 1 (Cons 2 (Cons 3 Nil)))

After each iteration we have

ys = Nil
ys = Cons 1 Nil
ys = Cons 2 (Cons 1 Nil)
ys = Cons 3 (Cons 2 (Cons 1 Nil))

So now the list is reversed.
-}
--
-- To pull off this trick in pure language where we can't mutate
-- state, we write a recursive "helper" function that takes an extra
-- argument that plays the role of the result variable.
rev' :: List a -> List a
-- So the reverse of a list xs, now defers to a helper function
-- revHelper which takes two arguments
-- The first is the list we wish to reverse, the second is the
-- resultant reversed list.
-- We start (as above) with the result being the empty list
rev' xs = revHelper xs Nil
  where
    revHelper :: List a -> List a -> List a
    -- If we've reached the end of the list we want to reverse, the
    -- result is in the second argument
    revHelper Nil ys = ys
    -- Otherwise, we pick off the next value from the list we want to
    -- reverse and cons it onto the front of our result list. This is
    -- an example of a tail recursive call (the last thing the
    -- recursive function does is to call itself). These have the nice
    -- property that they can trivially be rewritten into loop-like
    -- code by a compiler (and so can have an efficient
    -- implementation). Some languages (notably Scheme) mandate this
    -- transformation (called tail call elimination) is done, although
    -- Haskell does not.
    revHelper (Cons x xs) ys = revHelper xs (Cons x ys)
-- We can then check the speed at which this new version of reverse
-- executes, and find that it is now linear time.



-- More on recursion
-- We've already written a bunch of recursive functions, let's go
-- through a step-by-step example with a kind of systematic approach.

-- Let's implement the library function drop, which drops the first n
-- elements from a list. drop 2 [1, 2, 3] -> [3]

-- The steps we perform
-- 1. Write type
-- 2. Enumerate cases
-- 3. Handle simple cases
-- 4. Write recursive case (splitting the difficult problem into
--    easier bits)
-- 5. Simplify the patterns
drop :: Int -> [a] -> [a]
-- If we have an empty list we just return it
drop _ [] = []
-- If we have zero elements to drop, we return the list
drop 0 xs = xs
-- Otherwise we destructure the list and recurse on the tail.
drop n (x:xs) = drop (n - 1) xs

-- Have we handled all the cases? Is our function _total_. That is,
-- does it map from all possible inputs to an valid output?
-- With the hole for the empty list pattern, when we pass in a
-- negative number of entries to drop then we produce an empty list.
-- This seems wrong. The Haskell standard library defines dropping a
-- negative number of entries the same as dropping zero entries. This
-- also seems unsatisfactory.

-- Could we instead have written our function so that the types
-- guarantee that n >= 0?
-- Haskell's library functions don't usually take positive integers as
-- arguments. Some of this appears to be historical accidents, see the
-- answers to this question https://stackoverflow.com/q/12432154
--
-- One thing we can do is define our own positive integer using Peano
-- numbers (https://wiki.haskell.org/Peano_numbers)
-- This is an inductive definition of the natural numbers.
data Nat = Zero | Succ Nat
  deriving (Show, Eq)

-- Let's let ourselves convert from integers into our natural number type.
intToNat :: Integral a => a -> Nat
intToNat 0 = Zero
intToNat n | n < 0 = undefined
           | otherwise = Succ (intToNat (n - 1))

-- Now we can write drop and make it impossible to drop a negative
-- number of entries.
drop' :: Nat -> [a] -> [a]
drop' _ [] = []
drop' Zero xs = xs
drop' (Succ n) (x:xs) = drop' n xs

{- ASIDE:

-- This is rather ugly, and we have to go around and fix everything
-- everywhere to handle this new Nat type. Computing with it is also
-- rather slow! There are efforts to add better support for this kind
-- of thing, called "dependent types" to Haskell.
-- Heres an interesting article:
-- https://serokell.io/blog/why-dependent-haskell
-- There are also efforts to build languages with "refinement types"
-- on top of Haskell, for example Liquid Haskell
-- (https://ucsd-progsys.github.io/liquidhaskell-blog/)
--
-- These allow you to _refine_ the types in Haskell programs with
-- properties that allow us to check things like n >= 0 when passed to
-- drop. The checking is done at compile-time (rather than runtime) so
-- there's a possibility of faster code, as well as not having to
-- worry about runtime explosions of your rocket
-- http://www-users.math.umn.edu/~arnold/disasters/ariane.html

END ASIDE -}
