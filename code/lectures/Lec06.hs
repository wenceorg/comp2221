-- This magic comment enables a Haskell language extension in GHC that
-- allows me to write parallel generators in list comprehensions (I'll
-- flag where it is used below).
{-# LANGUAGE ParallelListComp #-}

module Lec06 where
import Prelude hiding (length, product, sum, maximum, and, const)

-- Let's look at some list comprehensions
-- These are analogous to set comprehensions in mathematics, sometimes
-- called set builder notation
-- https://en.wikipedia.org/wiki/Set-builder_notation

-- For example, we might write the set of all even positive integers
-- as
-- {i : i ∈ ℕ, i mod 2 = 0}
-- Or, indeed
-- {2*i : ∈ ℕ}

-- Haskell offers us something very similar to this, but for lists.
-- So one caveat to note is that we can have repeated values.
-- The expression to the left of the pipe symbol | is the "generator
-- expression"
-- To the right of the pipe we have "generators" which take the form
-- somePattern <- someList
-- We can also have boolean guards (expressions that refer to the
-- generated values with type Bool).
evenPositiveIntegers :: [Integer]
evenPositiveIntegers = [i -- Generator expression
                       | i <- [0..], -- generator
                         i `mod` 2 == 0 -- guard
                       ]

-- We can also use expressions in the value that is returned
evenPositiveIntegers' :: [Integer]
evenPositiveIntegers' = [2*i | i <- [0..]]

-- ASIDE: don't do that, do this
evens :: [Integer]
evens = [0,2..] -- haskell can figure out what you mean
-- END ASIDE.

-- In the live session there was some discussion on infinite lists and
-- I showed an infinite fibonacci sequence.
-- I'll defer to when we discuss lazy evaluation to cover that properly.

-- This comprehension is the one that needs the language extension for
-- parallel list comprehensions. Notice how there are two pipes
-- This says, walk down xs and ys simultaneously, and stop as soon as
-- you reach then end of the shortest list.
-- This is a list comprehension implementation of zip.
zipp :: [a] -> [b] -> [(a, b)]
zipp xs ys = [(x, y)
             | x <- xs
             | y <- ys]

-- Multiple generators are allowed, later generators vary fast
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

-- Like
{-
l = []
for x in xs:
    for y in ys:
        l.append((x, y))
-}

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
isPrime n | n <= 1    = False
          | otherwise =  factors n == [1, n]

-- All the primes up to n
-- In the exercises we look at a way to generate primes using trial
-- division on an infinite list
primes :: Int -> [Int]
primes n = [p | p <- [2..n], isPrime p]


--
-- iteration is "just" recursion with modification of local variables
-- taking the place of recursive calls.
-- So we can turn iteration into recursion by carrying around state as
-- extra arguments.
{-
def factorial(n):
    acc = 1
    for i in range(n, 1, -1):
        acc *= i
    return acc
-}
-- This example is kind of silly, we would probably write
-- product [1..n]
-- The idea here is that we have one local variable we're modifying
-- (res in the python above) which is an extra argument to our local
-- loop function.
factorial :: Integer -> Integer
factorial n = loop n 1
  where
    -- Recursive base case, just return the accumulator
    loop 1 acc = acc
    -- Recurse (next iteration of our loop) updating the accumulation
    -- parameter.
    loop i acc = loop (i - 1) (acc * i)

-- The natural recursive option is this one
-- We wondered in the lectures which would be faster. It seems that
-- this option is. But sometimes its convenient to write "loopy" code.
fact' :: Integer -> Integer
fact' 1 = 1
fact' n = n * fact' (n-1)


-- Having seen some list comprehensions, we'll now go to higher order
-- functions and think about how we can raise the level of abstraction
-- available in our programs.

-- Maps and folds

-- Let's think about abstracting a bunch of computations that we've
-- already done.

-- We've seen map, which takes f :: a -> b, xs :: [a],
-- and produces ys :: [b] by applying f to every element of xs.
-- We also saw (in Lecture 4) an applyInsideMaybe (which took an
-- f :: a -> b, an (x :: Maybe a), and produced a (y :: Maybe b).
-- In both cases, we can think of this higher-order function as
-- "lifting" the action of f to apply to a container type (lists are
-- the container in the map case, Maybe objects in the
-- applyInsideMaybe case).
-- We'll see how to make our Maybe type mappable next time.

-- Now let's look at some more common functions on lists and see if we
-- can spot a pattern.

-- All these functions walk down the list and combine each entry in
-- the list with the result of a recursive call to themselves, using a
-- specified binary operator. They also all have a base case with a
-- default value for the entry list.
-- The default value is the identity for the operator.

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
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + (length xs)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * (product xs)

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + (sum xs)

-- The common pattern is the binary operator
-- "A way of combining an a and a b to produce a b"
-- f :: (a -> b -> b)
-- An initial value
-- "The b to produce if we have nothing of type a" 
-- z :: b
-- A list of things to combine
-- "The as to combine"
-- xs :: [a]
-- And we produce a result of type b
myFold :: (a -> b -> b) -> b -> [a] -> b
-- On the empty list, we need to produce a value of type b
-- The only thing we have in our pocket is z, so let's use it
myFold _ z [] = z
-- Otherwise, we'll summarise the tail of the list by recursing
-- (giving us a value of type b)
-- Then we combine that with the head of the list x :: a
-- Using the provided binary operator f.
myFold f z (x:xs) = x `f` (myFold f z xs)

-- Now we can redefine our functions
-- For length the binary operator ignores the value and just uses 1.
-- We can write this with a lambda expression
length' = myFold (\_ y -> 1 + y) 0
-- Or more succintly using const :: a -> b -> a
-- const has the definition
const :: a -> b -> a
const x _ = x
-- So (const x) :: b -> a is a unary function which returns x
-- independent of the inputs
length'' = myFold (const (1+)) 0

-- These follow the pattern as well
and' = myFold (&&) True
product' = myFold (*) 1
sum' = myFold (+) 0

-- Finally we show maximum, which requires that the numbers admit a
-- total ordering (indicated by the Ord type class).
-- This is very similar except that it needs a list of a least one
-- element.
maximum :: (Ord a, Num a) => [a] -> a
maximum [] = undefined
maximum [x] = x
maximum (x:xs) = if x > tailmax
                 then x
                 else tailmax
  where tailmax = maximum xs

-- We'll see what the standard library offers for these patterns next time
