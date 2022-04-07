module Lec04 where
import Prelude hiding (Maybe, Just, Nothing)

-- Recall that we were looking at some manipulation of this "Maybe" a
-- result datatype.
data Maybe a = Nothing | Just a
  deriving Show

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
  (Just x) == (Just y) = x == y
  _ == _ = False

x :: Maybe (Maybe a)
x = Just Nothing

y = Nothing

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


-- We've seen a bunch of pattern matching so far, but let's talk about
-- it in more detail.

-- Patterns _destructure_ arguments matching the data constructor
-- mode.

-- For lists, the constructor is (:), called "cons", let's translate
-- this into our own data structure. Historical aside, the word cons
-- comes from Lisp see https://en.wikipedia.org/wiki/Cons
--
-- A linked list is an inductive data type which is either empty (or
-- Nil here), or else the Cons of a value and a further list.
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
append Nil Nil = Nil
append Nil ys = ys
append xs Nil = xs
append (Cons x xs) ys = Cons x (append xs ys)

-- And to reverse a list
-- This time we'll do a linear-time algorithm
-- Postponed to the next set of live coding
-- We did this as reverseFast in Lec05.hs
rev :: List a -> List a
rev = undefined
