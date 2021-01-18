module Lec03 where
import Prelude hiding (Maybe, Just, Nothing)
-- Curried functions

-- For functions of more than one argument, we have a choice of
-- whether to take the arguments one at a time (curried) or all at
-- once (uncurried) . Haskell prefers the former.

add :: Int -> Int -> Int
add x y = x + y

add' :: (Int, Int) -> Int
add' (x, y) = x + y

-- A simple example as to why.
-- Consider adding 1 to every entry of a list

xs :: [Int]
xs = [1, 4, 5]

ys :: [Int]
ys = map (add 1) xs

-- We actually don't need to define this function, and can use a
-- "section"
ys' :: [Int]
ys' = undefined

-- compare, with the uncurried version
zs :: [Int]
zs = undefined

-- Associativity
-- We now need some conventions on how tightly -> and function
-- application bind, and which way.
-- We could avoid this by writing lots of brackets.

tripleProduct :: Int -> (Int -> (Int -> Int))
tripleProduct x y z = x * y * z

xyz :: Int
xyz = (((tripleProduct 1) 2) 3)

-- This requires us to write lots of brackets 😞
-- Instead, make (->) right-associative, and function application
-- left-associative.

tripleProduct' :: Int -> Int -> Int -> Int
tripleProduct' x y z = x * y * z

xyz' :: Int
xyz' = tripleProduct 1 2 3

-- Be careful with this, if you use a function like this in a
-- higher-order context, e.g. our add function above:

-- foo = map add 1 xs -- Doesn't work


-- Polymorphism
-- Let's look at the type signature of `head`
-- Remember that at compile time, every value in a Haskell function
-- has a single (static) type.
--
-- As a consequence, what does this mean for the types of

intHead = (head [1, 2, 3]) :: Int

boolHead = (head [True, False]) :: Bool

-- These functions must have different types.

-- Let's see what Haskell has to say.
-- Prelude> :info head


-- Polymorphism

-- The nth value of a list
nth :: Int -> [a] -> a
nth = undefined


-- This function actually presents us with a problem, what should we
-- do for this call?
what = nth 100 [1, 2, 3]

-- We could have some sentinel value:
safeNth' :: Int -> a -> [a] -> a
safeNth' = undefined

-- But how do we distinguish between the sentinel value being in the
-- list, and a failure.

-- Haskell has a solution (and other languages also pick up the same
-- idea), namely we should have safe functions return "Maybe" a value

data Maybe a = Nothing | Just a
  deriving (Eq, Show)

-- Now we can write a safeNth

safeNth :: Int -> [a] -> Maybe a
safeNth = undefined

-- Why is this useful?
-- Can _safely_ distinguish "I failed" from "I succeeded".

-- Now let's consider manipulating these Maybe values, to see why this
-- might be a useful construct

-- Let's first do some recursive function definitions
-- Here's a polymorphic function
append :: [a] -> [a] -> [a]
append = (++)

rev :: [a] -> [a]
rev = reverse

-- Now let's think about something a little more complicated.
-- Consider chopping a prefix off the front of a list
-- So
-- chopPrefix [1, 2] [1, 2, 3, 4] == [3, 4]
chopPrefix :: Eq a => [a] -> [a] -> Maybe [a]
chopPrefix [] ys = Just ys
chopPrefix _ []  = Nothing
chopPrefix (x:xs) (y:ys) | x == y    = chopPrefix xs ys
                         | otherwise = Nothing

-- Then let's consider chopping a suffix off the end of a list
-- So chopSuffix [1, 2] [3, 1, 2] == [3]
chopSuffix :: Eq a => [a] -> [a] -> Maybe [a]
chopSuffix xs ys = chopPrefix (rev xs) (rev ys)

prefixFree = chopPrefix "hello" "hello.txt"


suffixFree = chopSuffix ".boat" "kayak.boat"

suffixBad = chopSuffix ".txt" "hello.txt" 
-- Uh-oh, something is not right

-- We need to be able to apply a function inside a Maybe type
applyInsideMaybe :: (a -> b) -> Maybe a -> Maybe b
applyInsideMaybe = undefined
