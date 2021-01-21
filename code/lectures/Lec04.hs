module Lec04 where
import Prelude hiding (Maybe, Just, Nothing)

-- Recall that we were looking at some manipulation of this "Maybe" a
-- result datatype.

data Maybe a = Nothing | Just a
  deriving Show


-- We're going to implement chopPrefix, which strips a prefix off a
-- list. But we want to do so safely, so that we can distinguish
-- between "everything was chopped off" and "the provided prefix was
-- not part of the list".
-- To do so, we'll want to return Maybe a list.
--
-- chopPrefix "" "abc" -> Just "abc"
-- chopPrefix "foo" "abc" -> Nothing
-- chopPrefix "ab" "abc" -> Just "c"

chopPrefix :: Eq a => [a] -> [a] -> Maybe [a]
chopPrefix [] ys = Just ys
-- The empty list has already matched above, so now we can put a hole
-- in the first pattern here.
chopPrefix _ [] = Nothing
chopPrefix (x:xs) (y:ys) | x == y = chopPrefix xs ys
                         | otherwise = Nothing

-- If we can chop off a prefix, it might be nice to be able to chop
-- off a suffix as well. The type signature should probably be the
-- same. We can implement this by using chopPrefix and some builtin
-- functions on lists.

chopSuffix :: Eq a => [a] -> [a] -> Maybe [a]
chopSuffix suffix ys = chopPrefix (reverse suffix) (reverse ys)

-- Let's check that it works

prefixFree = chopPrefix "hello" "hello.txt"

suffixFree = chopSuffix ".boat" "kayak.boat"

-- Everything looks good, or is it?

suffixFree' = chopSuffix ".boat" "row.boat"

-- How should we fix this?

-- Want to "lift" a -> b into (Maybe a -> Maybe b)
-- That is, given a function from (a -> b), we want to produce a new
-- function from (Maybe a -> Maybe b).
-- If we have Nothing, we can't do anything, so we just return
-- Nothing.
-- If we have (Just x) :: Maybe a, we need to turn this into
-- (Just y) :: Maybe b. So we have x :: a, and f :: a -> b, we can
-- therefore conjure y :: b, by applying f to x. We then must wrap it
-- up in Just.
-- We will see later why this definition is a "good" one.
applyInsideMaybe :: (a -> b) -> Maybe a -> Maybe b
applyInsideMaybe _ Nothing  = Nothing
applyInsideMaybe f (Just x) = Just (f x)

-- Now we can define chopSuffix using chopPrefix and reverse-ing the
-- final result inside the Maybe [a] we get back.
chopSuffix' :: Eq a => [a] -> [a] -> Maybe [a]
chopSuffix' suffix ys = applyInsideMaybe reverse chopped
  where chopped = chopPrefix (reverse suffix) (reverse ys)

-- We've actually already seen a function similar to applyInsideMaybe,
-- namely map :: (a -> b) -> [a] -> [b], which, using our naming might
-- be called "applyInsideList".
-- The pattern, which we'll see elsewhere, is of taking a function
-- between two "base" types (f :: a -> b) and "lifting" it to operator
-- on a container of those types (map f) :: [a] -> [b].



-- More polymorphism
-- Notice how I added this (Eq a => ) constraint on the types for
-- chopSuffix and chopPrefix. This is an example of constraining a
-- polymorphic function to types that only satisfy a particular
-- _interface_. In this case, the type must satisfy equality, by
-- providing an implementation of (==).

-- As another example, addition has the type signature
-- (+) :: Num a => a -> a -> a

-- "(+) operators on any type a, as long as that type satisfies the
-- interface required of the Num _type class_"

-- We can ask GHC what the Eq type class wants of us using
-- :info Eq
-- This also prints out all the currently known types that _implement_
-- this class.

-- And let us now see how to implement it for our Maybe datatype.
-- We have to write an _instance_ of the type class for our data type.

-- To do so, we declare an instance. If we want equality for types
-- (Maybe a), we had better have equality for a itself. So we say that
-- this instance declaration produces equality for (Maybe a) as long
-- as a provides equality.
-- We then just define (==) inside the block.
--
-- You may also have noticed that I've previously been defining
-- datatypes and just doing "deriving Eq" on the definition. This uses
-- GHC's internal representation to _automatically_ construct the Eq
-- instance for the datatype (and will turn out something isomorphic
-- to this definition).

instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x  == Just y  = x == y
  _       == _       = False

{- ASIDE: We could also have done this like so

eqMaybe :: Eq a => Maybe a -> Maybe a -> Bool
eqMaybe Nothing  Nothing  = True
eqMaybe (Just x) (Just y) = x == y
eqMaybe _        _        = False

instance Eq a => Eq (Maybe a) where
  (==) = eqMaybe

-- At compile time, when the types are inferred and functions are
-- monomorphised, GHC does the equivalent of substituting the
-- ad-hoc polymorphic function (==) with the specific implement
-- eqMaybe.

END ASIDE -}


-- We had a question in the lectures, of whether we can use this
-- facility to (say) implement concatenation of Char values using (+).
-- That is, could we implement a Num instance for Char so that we can
-- write 'a' + 'b' and have it turn into "ab".
--
instance Num Char where
-- This is not possible in two ways. First, we must implement a number
-- of functions for the Num type class, and it's hard to know what to
-- do for many of them. OK, let's just leave them undefined like we
-- have here.
  (-) = undefined
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
-- Now we come to (+) :: Num a => a -> a -> a
-- Or, monomorphising to Char:
--
-- plusChar :: Char -> Char -> Char
--
-- Here we have a problem, because the concatenation of two Chars is
-- not a Char, but rather a String (or list-of-Char).
-- So we can't write a type class instance for Num Char because we
-- can't satisfy the type constraint on (+).
--
-- A fancier way of saying this is that concatenation of Chars is not
-- a closed operation (see
-- https://en.wikipedia.org/wiki/Closure_(mathematics)).
-- Num requires this of our definitions, so we can't implement such a
-- type class for Char.
  -- Doesn't work, because concatenation of characters is not closed
  -- over characters.
  (+) c d = undefined

-- We've seen a bunch of pattern matching so far, but let's talk about
-- it in more detail.

-- Patterns _destructure_ arguments matching the data constructor
-- mode.

-- For lists, the constructor is (:), called "cons", let's translate
-- this into our own data structure. Historical aside, the word cons
-- comes from Lisp see https://en.wikipedia.org/wiki/Cons
--
-- A linked list is an inductive data type which is either empty (or
-- Nil) here, or else the Cons of a value and a further list.
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- Pictorially, the list
list = Cons 1 (Cons 2 Nil)
-- Looks like
--      Cons                  Cons
-- +---+--------+        +---+--------+        +-----+
-- | 1 | List a |   +--->| 2 | List a |   +--->| Nil |
-- +---+---+----+   |    +---+---+----+   |    +-----+
--         |        |            |        |
--         +--------+            +--------+

-- This is actually the storage format for Haskell's builtin list,
-- where the empty list Nil is called [], and the cons operator is
-- called (:).
-- Compare
-- :type Cons with :type (:)
-- and
-- :type Nil with :type []

-- Let's write an append function to append two lists together
append :: List a -> List a -> List a
-- If the first list is Nil (or empty) then we should probably just
-- return the second one.
append Nil ys = ys
-- If the first list is not Nil, it must be composed of a head, and a
-- tail which we deconstruct by matching on the data constructor Cons
-- We need to move the xs onto the ys, so we return a new list by
-- appending ys to the tail and consing on the head
append (Cons x xs) ys = Cons x (append xs ys)

-- Exactly the same definition can be applied to the builtin list,
-- only now the pattern matches are spelt slightly differently.
-- You can check that this is the same as the standard library
-- definition here
-- https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.Base.html#%2B%B2
append' :: [a] -> [a] -> [a]
append' [] ys = ys
append' (x:xs) ys = x:(append' xs ys)

-- We didn't do this one
rev :: List a -> List a
rev = undefined
