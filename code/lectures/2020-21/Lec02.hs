module Lec02 where
import Prelude hiding (Bool, True, False, not)

-- This syntax means "x has type Int"
-- All variables are spelt with a lowercase letter at the start.
-- All types are spelt with an uppercase letter at the start.
x :: Int
x = 10

y :: Int
y = 11

a :: Int
a = x + y

-- Characters with single quotes
someChar :: Char
someChar = 'c'


-- This is a type synonym
-- type String = [Char]
-- Strings with double quotes
someString :: String
someString = "abc"

-- Arbitrary-precision integers
bignum :: Integer
bignum = 2^20000

-- Haskell is very strict about types, there's no implicit coercion
-- between numbers of different types (for example)
c :: Double
c = 12.0

-- This won't work
-- d = a + c

-- But we can turn "ints" into numbers and then they can be cast down
d :: Double
d = (fromIntegral a) + c


-- What if we don't want to introduce global bindings? Two options.
-- 1. We have an _expression_. Introduce bindings with "let"
-- 2. We have a syntactic block. Introduce bindings with "where"
-- (--> later)

-- Syntax: let {assignments} in {expression}
e :: Int
e = let x = 1
        y = 2
        z = x + y
    in x + y + z

-- For short things, we can also write it on one line
e' :: Int
e' = if (let {x = 1; y = 2} in x*y == 2) then 10 else 1
{-

ASIDE: layout rule

You might be noticing by now that "idiomatic" Haskell uses quite short
variable names, and whitespace to show structure.

The above definition of `e` makes use of the "layout rule".

A let can introduce many bindings, as we see above.

The layout with indentation of all the names lining up is necessary
for correctness.
-}

{- This won't work
e' :: Int
e' = let x = 1
       y = 2
         z = x + y
     in x + y + z
-}

-- With the layout rule, the correct whitespace can be "desugared"
-- (because it looks less sweet) into. Don't write this, it is frowned upon.
e'' :: Int
e'' = let {x = 1;
          y = 2;
          z = x + y;}
      in x + y + z

-- Why do I need all these types in my program? I don't have to write
-- them in Python!

-- Needed to execute. For example integer division 10 / 2, is
-- implemented with _different_ instructions in the CPU than floating
-- point division 10.0 / 3.0. Something needs to know to emit the
-- correct instruction (impossible if we don't know how to interpret
-- the stream bits that our program is when it reaches the hardware).

-- What is a type?
-- A collection of values

-- Bool is a type with two values, True and False.
data Bool = True | False
  deriving Show -- Automatically make printable.

-- The values are said to _inhabit_ the type

-- Similarly to mathematics, Haskell uses -> to indicate a function
-- type.
-- Let's implement logical things on our Bool type
-- "not eats a Bool value and returns a Bool value"
not :: Bool -> Bool
not True = False
not False = True

-- The :type command will ask GHCi what it thinks the type of an
-- expression is.

-- We saw some lists already
-- "xs has type list-of-Bool".
-- In general, for some t, xs :: [t] means "xs has type list-of-t"
xs :: [Bool]
xs = [True, False, True]

-- Here's a list of lists
-- Note how the type does not refer to the length.
xss :: [[Int]]
xss = [[1], [2, 3, 4], [4, 5]]


-- There are also tuple types, whose type _does_ encode the size
tuple :: (Bool, [Bool])
tuple = (False, xs)


-- Now let's look at function types, you may not think about this
-- particularly in other languages, but it's pretty core in Haskell.

-- Let's implement a binary function "xor", whose truth table is
--
--   F F => F
--   T F => T
--   F T => T
--   T T => F

-- How do we provide two arguments?
-- Curried, and uncurried

{- ASIDE:

Currying is not named after food, but rather after Haskell Curry
(https://en.wikipedia.org/wiki/Haskell_Curry) (as is Haskell itself)/

He did foundational work in lambda calculus and the ideas of "programs
as proofs"
(https://en.wikipedia.org/wiki/Curry–Howard_correspondence). These
days there are many functional programming languages (some built on
top of Haskell) for the _automation_ of mathematical proofs.
For example

* Agda: https://agda.readthedocs.io/
* Coq: https://coq.inria.fr
* Lean: http://leanprover.github.io

For a fun revisit of some of first year mathematics, Kevin
Buzzard has written the "Natural Number Game", where you use Lean
to prove properties of the natural numbers (0, 1, 2, ...)
https://wwwf.imperial.ac.uk/~buzzard/xena/natural_number_game/

-}

-- This is the uncurried version, it takes all the arguments at once,
-- packed up in a tuple and returns a Bool.
xorUncurried :: (Bool, Bool) -> Bool
xorUncurried (True, False) = True
xorUncurried (False, True) = True
xorUncurried _             = False

-- Here is the curried version, it takes arguments one at a time, so
-- really, it's a function which takes a Bool and returns a function
-- from Bool -> Bool
-- The arrow -> is right-associative, so we don't need the brackets
-- here, but I put them in for clarity.
xor :: Bool -> (Bool -> Bool)
xor True False = True
xor False True = True
xor _ _        = False

-- Which is better? And why?

-- Generally, the curried form is preferred.
-- Suppose we want to carry around a partially applied `xor` where we
-- know the first argument.
-- With the curried version, it is easy
xorPartial :: Bool -> Bool
xorPartial = xor True

values :: [Bool]
values = [True, False, True, True]
xors = map xorPartial values
-- equivalently
xors' = map (xor True) values

-- With the uncurried version, we have to explicitly provide a name
-- the variable that will go in the second slot of the argument.
xorUncurriedPartial :: Bool -> Bool
xorUncurriedPartial = \x -> xorUncurried (True, x)

-- So if we want to use it in a larger function, we either have to
-- define a name, like above, or introduce a lambda to capture the
-- variable, which is kind of messy.
xors'' = map (\x -> xorUncurried (True, x)) values

-- We call this idea of invoking a function with some (but not all) of
-- its arguments _partial application_. Building more complicated
-- programs by composition of (partially applied) functions is one of
-- the things we're going to do a lot. The curried form of functions
-- makes this easier to read and do, so that's why (expect where it is
-- not possible)
