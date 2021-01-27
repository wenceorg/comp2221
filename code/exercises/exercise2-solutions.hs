module Exercise2 where

x1 :: [Char]
x1 = ['a', 'b', 'c']

x2 :: (Char, Char, Char)
x2 = ('a', 'b', 'c')

-- This is a valid option
x3 :: [(Bool, Int)]
-- At the REPL
-- GHC infers the constrained polymorphic type
-- Num a => [(Bool, a)]
-- This means that the second entry in the tuple is a number. Bare
-- numbers like this are inferred to be polymorphic until such time as
-- they are used in a context where a concrete type is required.
-- When the module is loaded it infers [(Bool, Integer)]. I suppose
-- because the compilation needs to figure out how much storage is
-- required.
x3 = [(False, 0), (True, 10)]

-- This time we have (so tuple on the outside, list on the inside).
-- Again Num a => ([Bool], [a]) is also fine
x4 :: ([Bool], [Int])
x4 = ([False, True], [0, 1])

-- All of these are functions from [a] -> [a], so this has type list
-- of [a] -> [a]. Notice that we can't put arbitrary functions in
-- here, we can't additionally have head :: [a] -> a, because it has
-- the wrong type.
x5 :: [[a] -> [a]]
x5 = [tail, reverse, init]

-- The only thing we know about x and y is that they may have
-- different polymorphic types so this is
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- For pair, we take the arguments one at a time (curried)
pair :: a -> b -> (a, b)
pair x y = (x, y)

-- This one is also a constrained polymorphic function
-- Multiplication requires that the value is a numeric type, so we
-- have
square :: Num a => a -> a
square x = x*x

-- This one also has a constraint, this time, that the elements of the
-- list are comparable
palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

-- palindrome [tail]
-- does not type check, because function types do not implement
-- equality. The exercise asksL
-- Is Haskell right to complain that it can’t check that functions are
-- equal? Explain your reasoning?
--
-- I think Haskell is correct to complain. Although we can look at
-- some source code and reason about the functions it is quite
-- difficult for us. Let us think about what it would mean for two
-- functions to be equal: it must mean that whereever we see the use
-- of the first function f, we can replace it with the second function
-- g. In a pure language like Haskell, this is a valid substitution to
-- make (due to referential transparency
-- https://en.wikipedia.org/wiki/Referential_transparency).
-- If I only have the functions as a black box, then the only way I
-- can determine if they are equal is to check if for every input to
-- f, I get the same answer if I provide it as an input to g.
-- This already is a problem, since think of a simple function that
-- squares its argument. I can't check all inputs, since there are an
-- infinite number of integers.
--
-- Furthermore, the only functions we have a chance of checking is
-- ones that are guaranteed to return an answer at all. That is both f
-- and g must be computable
-- (https://en.wikipedia.org/wiki/Computable_function). In Haskell, I
-- can write functions which don't return: we might say that two
-- functions are equivalent if they both don't return but determining
-- if a function returns is equivalent (in a Turing complete language)
-- to solving the halting problem
-- (https://en.wikipedia.org/wiki/Halting_problem) which Turing proved
-- is not possible.
-- That's a long way of saying that Haskell is right to complain!

-- We determine from the use of `f` on the right hand side that it is
-- a function of one argument. We don't know any more, so it must be
-- polymorphic.
-- This time we have to parenthesise the first (a -> a) to indicate
-- the the first argument is a function type.
twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- Now some list manipulation, we were asked to write butlast which
-- returns the penultimate element of a list of at least two entries

-- First with builtin functions, we notice that if we reverse a list,
-- take its tail and then take the head of that, we get the
-- penultimate element. This will error if the list does not contain
-- two or more elements.
butlast :: [a] -> a
butlast xs = head (reverse (tail xs))

-- We can also write this in pointfree style
-- See https://github.com/wenceorg/comp2221/discussions/14 for more
-- information
butlast' :: [a] -> a
butlast' = head . reverse . tail

-- We were also asked to write this with pattern matching and
-- recursion, let's have a go
butlast'' :: [a] -> a
-- If we have a two-element list, then we are at the base case and can
-- return the first element
butlast'' [x, _] = x
-- If we have a non-empty-list of more than two elements then let's
-- recurse. This pattern actually matches the single element list [_],
-- for which we have said the answer is undefined, but the recursion
-- then ends up calling butlast'' [] which is cleaned up by the next
-- pattern.
-- If we wanted to explicitly handle the single-element case, we would
-- add a pattern butlast'' [_] = undefined
butlast'' (x:xs) = butlast'' xs
-- Finally we need to handle the patterns that we haven't matched yet
-- This is only the empty list, for which we are undefined
-- We can match on a hole here if we like
butlast'' _ = undefined

-- Both of these implementations walk down the list once, and so they
-- are both O(n) in the length of the list. That is, the amount of
-- time it takes grows linearly with the length.


-- Finally we are to write safetail in three different ways
-- First with conditionals
-- This is reasonably straightforward. The only thing to note is that
-- we should use the function null :: [a] -> Bool to check for the
-- input list being empty. DO NOT use (length xs == 0) for checking
-- the emptiness of a list, since this always has to traverse to the
-- end of the list. It is inefficient for finite-sized lists, and does
-- not terminate for infinite lists.
-- That is, (null [1..]) returns False immediately,
-- whereas (length [1..] == 0) never returns.
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- With guard expressions we have a similar story
-- Again, note the use of null to check emptiness
safetail' :: [a] -> [a]
safetail' xs
  | null xs   = []
  | otherwise = tail xs


-- Finally with pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
-- We can destructure here
-- The other option would be
-- safetail'' xs = tail xs
-- In that case, we would need to make sure that this pattern comes
-- second.
safetail'' (_:xs) = xs

-- Finally let's look at another way of doing a guard-like approach,
-- using a case expression. This is a slightly desugared way of doing
-- pattern matching, but where we can call a function and match on the
-- result. This not so useful here, the difference is that the case is
-- an _expression_ so we can use it inside a larger statement if we
-- like. For a little bit more description, see
-- http://learnyouahaskell.com/syntax-in-functions
safetail''' :: [a] -> [a]
safetail''' xs = case null xs of
                   True -> []
                   _ -> tail xs
