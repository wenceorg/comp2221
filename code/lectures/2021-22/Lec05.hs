-- This magic comment enables a Haskell language extension in GHC that
-- allows me to write parallel generators in list comprehensions (I'll
-- flag where it is used below).
{-# LANGUAGE ParallelListComp #-}

module Lec05 where

import Prelude hiding (and, concat, const, drop, length, maximum, product, sum, zip, lookup)
import GHC.Float (expts10)

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
drop n [] = []
drop 0 xs = xs
drop n (_ : xs) = drop (n-1) xs

-- Question: does drop work "correctly" if n < 0?

firstSingleton :: [a] -> a
firstSingleton [x] = x

firstTwo :: [a] -> (a, a)
firstTwo (x:y:zs) = (x, y)

-- What can't I pattern match?
-- Can only use "constructors", so this definition of last is
-- appealing, but doesn't work
-- last :: [a] -> a
-- last (xs ++ [x]) = x

last' :: [a] -> a
last' [x] = x
last' [] = undefined
last' (_:xs) = last' xs

last'' :: [a] -> a
last'' xs = head (reverse xs)

-- You might be worried that the only thing you can do is write
-- recursive functions, not so!

-- Let's look at some list comprehensions
-- These are analogous to set comprehensions in mathematics, sometimes
-- called set builder notation
-- https://en.wikipedia.org/wiki/Set-builder_notation

-- For example, we might write the set of all even positive integers
-- as
-- {i : i ∈ ℕ, i mod 2 = 0}
-- Or, indeed
-- {2*i : i ∈ ℕ}

-- Haskell offers us something very similar to this, but for lists.
-- So one caveat to note is that we can have repeated values.
-- The expression to the left of the pipe symbol | is the "generator
-- expression"
-- To the right of the pipe we have "generators" which take the form
-- somePattern <- someList
-- We can also have boolean guards (expressions that refer to the
-- generated values with type Bool).
evenPositiveIntegers :: [Integer]
evenPositiveIntegers = [i -- generator expression
                       | i <- [0..] -- iteration over some list
                       , even i -- conditions
                       ]

-- We can also use expressions in the value that is returned
evenPositiveIntegers' :: [Integer]
evenPositiveIntegers' = [2 * i | i <- [0..]]

-- ASIDE: don't do that, do this
evens :: [Integer]
evens = [0, 2..] -- haskell can figure out what you mean
-- END ASIDE.

-- This comprehension is the one that needs the language extension for
-- parallel list comprehensions. Notice how there are two pipes
-- This says, walk down xs and ys simultaneously, and stop as soon as
-- you reach then end of the shortest list.
-- This is a list comprehension implementation of zip.
zip :: [a] -> [b] -> [(a, b)]
zip xs ys = [(x, y) | x <- xs | y <- ys]

-- Multiple generators are allowed, later generators vary faster
cartesianProduct :: [a] -> [b] -> [(a, b)]
-- Note the difference from the zip above.
cartesianProduct xs ys = [(x, y) | y <- ys, x <- xs]

-- Nested lists are also supported
concatComprehension :: [[a]] -> [a]
concatComprehension xss = [x | xs <- xss, x <- xs]

-- Variable bindings in the generators can be used in later
-- expressions
restricted :: Int -> [(Int, Int)]
-- All pairs (x, y) with x, y ∈ [0..n] with y >= x
restricted n = [(x, y) | x <- [0..n], y <- [x..n]]

-- Let's write a really bad prime number generator
-- The factors of an integer are all integers f such that n mod f == 0
factors :: Int -> [Int]
factors n = [f | f <- [1..n], n `mod` f == 0]

-- A prime number is >= 2 and has as its factors 1 and itself.
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = factors n == [1, n]

-- All the primes up to n
-- In the exercises we look at a way to generate primes using trial
-- division on an infinite list
primes :: Int -> [Int]
primes n = [p | p <- [0..n], isPrime p]

--
-- iteration is "just" recursion with modification of local variables
-- taking the place of recursive calls.
-- So we can turn iteration into recursion by carrying around state as
-- extra arguments.
-- Let's see this in action to reverse a list "fast"
--
-- The natural way to reverse a linked list in an imperative language
-- is like so:
--
-- reversed = []
-- for head in input:
--   reversed.insert(0, head)
--
-- How can we do this in a language where we can't modify variables?

reverseSlow :: [a] -> [a]
reverseSlow [] = []
reverseSlow (x:xs) = reverseSlow xs ++ [x]

reverseFast :: [a] -> [a]
reverseFast xs = go xs []
  where
    go :: [a] -> [a] -> [a]
    go [] acc = acc
    go (x:xs) acc = go xs (x:acc)


-- Having seen some list comprehensions, we'll now go to higher order
-- functions and think about how we can raise the level of abstraction
-- available in our programs.

-- The goal here is to think about capturing patterns of computation
-- in reusable pieces.

-- Maps and folds

-- Let's think about abstracting a bunch of computations that we've
-- already done.

-- A pattern we've already seen is map

-- map :: (a -> b) -> [a] -> [b]

-- and we noted that it was similar to applyInsideMaybe (which we
-- might call mapMaybe)

-- mapMaybe (a -> b) -> Maybe a -> Maybe b

-- In both cases, we can think of this higher-order function as
-- "lifting" the action of f to apply to a container type (lists are
-- the container in the map case, Maybe objects in the
-- mapMaybe case).

-- We'll see how to make our Maybe type mappable next time.

-- Now let's look at some more common functions on lists and see if we
-- can spot a pattern.

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

-- What's the common pattern?

-- All these functions walk down the list and combine each entry in
-- the list with the result of a recursive call to themselves, using a
-- specified binary operator. They also all have a base case with a
-- default value for the entry list.
-- The default value is the identity for the operator.


myFold :: (a -> b -> b) -> b -> [a] -> b
myFold f z [] = z
myFold f z (x:xs) = x `f` (myFold f z xs)

-- Now we can redefine our functions
-- For length the binary operator ignores the value and just uses 1.
-- We can write this with a lambda expression
length' = myFold (\_ acc -> 1 + acc) 0

-- Or more succintly using const :: a -> b -> a
-- const has the definition
const :: a -> b -> a
const x _ = x

-- So (const x) :: b -> a is a unary function which returns x
-- ignoring its input
length'' = myFold undefined

-- These follow the pattern as well
and' = myFold (&&) True

product' = myFold (*) 1

sum' = myFold (+) 0

-- Finally we show maximum, which requires the type admits a
-- total ordering (indicated by the Ord type class).
-- This is very similar except that it needs a list of a least one
-- element.
maximum :: Ord a => [a] -> a
maximum = undefined


-- What is this called in the standard library? Can you find it via hoogle.haskell.org?

{- ASIDE:
 If we want to think about this in maths terms, we're using a
 Monoidal structure (https://en.wikipedia.org/wiki/Monoid) to define
 what to do.

 A Monoid is a set (for length the set is integers) along with an
 associative binary operation (here +), and an identity element for
 the binary operation: 0 is the identity for + since x + 0 = x.

 For and, the set is {True, False}, the operator is logical
 conjunction (AND-ing) and the identity is True (since x ∧ True = x)

 For product, the set is the set of numbers,
 the operator is multiplication,
 and the identity is 1 (since x * 1 = x)

 For sum, the set is the set of numbers,
 the operator is addition and the identity is 0 (since x + 0 = x)

END ASIDE -}
