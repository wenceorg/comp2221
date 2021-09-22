module Lec03 where
-- We're going to redefine these.
import Prelude hiding (Maybe, Just, Nothing)

-- Curried functions (a reprise from the end of Lec02.hs)

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

-- We actually don't need to define this function, and can use an
-- "operator section". (+1) is equivalent to \x -> x + 1
-- Similarly (1+) is equivalent to \x -> 1 + x
-- This works for (*) and (/) in the same manner, but (-) is more
-- difficult. -expr Is defined as unary negation in the language
-- standard. And so we can't use a section for this. See
-- https://stackoverflow.com/a/28858218 for details.
ys' :: [Int]
ys' = map (+1) xs

-- compare with the uncurried version
-- Here I explicitly had to introduce a name for the second argument
-- to add' because there's no way to construct the partial application
-- of add' to its first argument: I need to be able to construct the
-- whole tuple.
zs :: [Int]
zs = map (\x -> add' (1, x))  xs

-- Associativity and binding of -> and function application.
-- We now need some conventions on how tightly -> and function
-- application bind, and which way.
-- The natural consequence of curried function application is that ->
-- associates to the right: like this.
tripleProduct :: Int -> (Int -> (Int -> Int))
tripleProduct x y z = x * y * z

-- And function application associates to the left, like this.
xyz :: Int
xyz = (((tripleProduct 1) 2) 3)

-- With these conventions, we can remove all the brackets. Which makes
-- things a little cleaner to read. Although we should recall the way
-- to read this is as above with the parentheses.
tripleProduct' :: Int -> Int -> Int -> Int
tripleProduct' x y z = x * y * z

xyz' :: Int
xyz' = tripleProduct 1 2 3

-- Be careful with this, if you use a function like this in a
-- higher-order context, e.g. our add function above:

-- foo = map add 1 xs
-- Doesn't work, because it's parsed as ((map add) 1) xs

-- ASIDE: (map add xs) works fine. Can you figure out what it is
-- doing?

-- Polymorphism.

-- Let's look at the type signature of `head`
-- Remember that at compile time, every value in a Haskell function
-- has a single (static) type.
-- As a consequence, what does this mean for the types of
intHead = (head [1, 2, 3]) :: Int
boolHead = (head [True, False]) :: Bool

-- The values have different types, and so when we called the `head`
-- function, it must have been two different types for the two
-- different lists?
-- These functions must have different types.
-- Let's see what Haskell has to say.
-- Prelude> :type head
-- head :: [a] -> a

-- Notice here how the names in the type declaration start with
-- lowercase letters. Recall that all concrete types (like Int, Bool,
-- etc...) are named with uppercase letters. These lowercase names are
-- called "type variables" and they stand for any concrete type.
-- So this says "hd" takes a list of _any_ type "a", and returns a value
-- which is also of type "a".
-- This function is therefore "polymorphic" (of many kinds), and is
-- the way we can write generic code in Haskell. We're saying that hd
-- operates on a list of any type, and the return value is of the same
-- type as the list entries.
hd :: [a] -> a
hd [] = undefined
hd (x:_) = x


-- When the Haskell compiler encounters this code called at a concrete
-- type (for example Bool), it performs a process called
-- monomorphisation. Producing a concrete function with specialised
-- types as if we had written the below:
hdBool :: [Bool] -> Bool
hdBool [] = undefined
hdBool (x:_) = x
-- This is similar to the way C++ templates (which you might have seen
-- already) or Java generics (which you'll see later this term) work.


-- Notice how above we defined the hd function to raise an exception
-- if called on an empty list. This is similar to head in the standard
-- Prelude, which also raises an exception.
--
-- What if instead, we want to be safe (i.e. never raise an exception).

-- Let's illustrate this concept now by considering another function,
-- "nth", which is to return the nth entry in a list.
-- Again, it's polymorphic (we don't care what the type in the list
-- is, we just know that the return value is the same).
-- Indexing from 0 (as every sane person does) there's a natural
-- recursive definition. If the index is zero, we return the head of
-- the list, otherwise we recurse on the tail of the list, subtracting
-- one from the index.
-- The case of an empty list presents us with a problem, however. What
-- should we do when asking for the nth entry of an empty list?
-- Here we have left it undefined, but this is unsatisfactory.
nth :: Int -> [a] -> a
nth n (x:xs) | n == 0    = x
             | otherwise = nth (n-1) xs
nth _ [] = undefined

-- One way to phrase this problem is that our function is not _total_
-- (see: https://en.wikipedia.org/wiki/Partial_function). That is, it
-- does not produce valid answers for all valid inputs.
-- We could define away some inputs as being invalid, but the type
-- system has no way of specifying that the index to our function must
-- be in bounds (and we still can't handle the case of an empty list).
-- What should we do for this call?
what = nth 100 [1, 2, 3]
-- And what about this one?
what' = nth 0 []

-- ASIDE: the former problem _can_ be solved in some more
-- sophisticated type systems, which support _dependent types_ (where
-- types can refer to values). END ASIDE.


-- We could have some sentinel value. You might use something like
-- this in a Python function that takes keyword arguments so that you
-- can figure out if someone explicitly passed the argument in.
-- For example
--
-- sentinel = object()
-- def fn(*args, some_kwarg=sentinel):
--    if some_kwarg == sentinel:
--       some_kwarg = whatever_default
--
-- Often you would have written some_kwarg=None, but this would not
-- allow you to distinguish between
--
-- fn(some_kwarg=None) and fn()
--
-- Whereas the approach with a sentinel does.

-- So let's try that here. We have a problem, because the sentinel
-- value must have the same type as the entries in the list.
safeNth' :: Int -> a -> [a] -> a
safeNth' n sentinel (x:xs) | n == 0 = x
                           | otherwise = safeNth' (n-1) sentinel xs
safeNth' _ sentinel [] = sentinel

-- But how do we distinguish between the sentinel value being in the
-- list, and a failure. We can't therefore distinguish between

accessed = safeNth' 1 (-1) [1, -1, 2]
-- and
failed = safeNth' 10 (-1) [1, 2, 3]


-- The solution in Haskell (and other languages also pick up the same
-- idea) is to have an Option type that represents a computation that
-- may fail. Rust calls this Option<T>, C++ calls it std::optional<T>.

-- In Haskell, it is called Maybe. (Normally just available in the
-- standard Prelude, or via import Data.Maybe).
data Maybe a = Nothing | Just a
  deriving (Eq, Show)

-- Now we can write a safeNth, which returns a (Maybe a)
-- When we succeed, we return "Just x", when we fail, we return
-- Nothing.
safeNth :: Int -> [a] -> Maybe a
safeNth n (x:xs) | n == 0 = Just x
                 | otherwise = safeNth (n-1) xs
safeNth _ [] = Nothing

-- Why is this useful?
-- Can _safely_ distinguish "I failed" from "I succeeded".

-- Now let's consider manipulating these Maybe values, to see why this
-- might be a useful construct

-- Let's first do some recursive function definitions
-- Here's a polymorphic function
append :: [a] -> [a] -> [a]
append = (++)

rev :: [b] -> [b]
rev = reverse

-- Now let's think about something a little more complicated.
-- Consider chopping a prefix off the front of a list
-- So
-- chopPrefix [1, 2] [1, 2, 3, 4] == [3, 4]
chopPrefix :: Eq a => [a] -> [a] -> Maybe [a]
chopPrefix [] ys = Just ys
chopPrefix _ [] = Nothing
chopPrefix (x:xs) (y:ys) | x == y    = chopPrefix xs ys
                         | otherwise = Nothing

-- chopPrefix [1, 2] [1] & chopPrefix [1, 2] [1, 2]
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
