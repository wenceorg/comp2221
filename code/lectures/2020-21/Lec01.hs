-- This module scopes the declarations of all our
-- definitions. It is not strictly required, but I do it because it
-- then plays nicer with my editor integration.
module Lec01 where

-- Import the standard library (this is done by default)
-- But don't import the filter function (we will implement it
-- ourselves)
import Prelude hiding (filter)

-- A list of integers
-- Compare to the mathematical spelling
-- xs : \mathbb{N}
xs :: [Int]
xs = [11, 3, 25, -1, 7]

-- All values in a list must have the same type
-- ys = [11, 'c']

-- Haskell will do type inference for you?
zs = ['c', 'd']

-- A filtered list, what is strange about this definition?
-- filter is undefined right now, so in a language like Python, this
-- would produce an error. Here it is fine, because evaluation is
-- delayed until such time as the value is needed.
filtered :: [Int]
filtered = filter (<10) xs

-- compare to Python filter(lambda x: x < 10, xs)

-- A first data type
-- We're introducing a new type, Direction, which has four values,
-- North, South, East, and West.
-- These are called "constructors" for the type.
-- The deriving Show line tells Haskell to construct a default
-- printable representation of the values. So that we can do `show
-- North` to produce the string representing the value.
data Direction = North | East | South | West
  deriving Show -- Make it printable

-- Haskell functions are "equational", they define how to replace the
-- thing on the left with thing thing on the right
reverseDirection :: Direction -> Direction
reverseDirection North = South
reverseDirection East = West
reverseDirection South = North
reverseDirection West = East

-- We can take more than one argument
-- Match on patterns for the data constructor. The symbol "_" is
-- called a "hole" and is a catchall pattern.
directionsEqual :: Direction -> Direction -> Bool
directionsEqual North North = True
directionsEqual East East = True
directionsEqual South South = True
directionsEqual West West = True
directionsEqual _ _ = False

-- Create some data (variables)
up = North
down = reverseDirection up

-- Function calls don't use parentheses.
same = directionsEqual up down

{- Now we have a multi-line comment.

 A filter function, we're replacing the standard library definition,
 which is why we hid it on import above.
 We'll implement it now

 filter takes a predicate function, a list, and returns a new list
 only containing those entries for which the predicate is True.
-}
filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x       = x:filter p xs
                | otherwise = filter p xs
                

-- If we want to run as a program we need to define a main function.
-- the main function does IO (input/output) and doesn't return a
-- value.
-- We will see details of this later in the course.
main :: IO ()
main = do
  putStrLn (show xs)
  putStrLn (show filtered)
  putStrLn (show up ++ " " ++ show down ++ " " ++ show (reverseDirection East))
