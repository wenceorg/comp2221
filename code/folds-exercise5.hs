module Exercise5 where

-- Linear recursive functions with folds

-- With foldr
length' :: [a] -> Int
length' = undefined

map' :: (a -> b) -> [a] -> [b]
map' = undefined

reverse' :: [a] -> [a]
reverse' = undefined

and' :: [Bool] -> Bool
and' = undefined

-- And with foldl
length'' :: [a] -> Int
length'' = undefined

map'' :: (a -> b) -> [a] -> [b]
map'' = undefined

reverse'' :: [a] -> [a]
reverse'' = undefined

and'' :: [Bool] -> Bool
and'' = undefined

-- Now some more higher-order functions.

-- Every entry satisfies a predicate
all' :: (a -> Bool) -> [a] -> Bool
all' = undefined

-- At least one entry satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
any' = undefined

-- Return elements from a list while they satisfy a predicate, and
-- stop as soon as the predicate is not satisfied
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = undefined

-- Remove elements from a list while they satisfy some predicate, and
-- then return the remainder. This one is MUCH HARDER, if you want to
-- have a go, follow along with this paper
-- https://wiki.haskell.org/wikiupload/1/14/TMR-Issue6.pdf
-- You would probably write this one recursively because it's much easier.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = undefined

-- You might have implemented all' with
-- all' p xs = and (map p xs)
-- This approach traverses the list twice, but noting the definition
-- of map using foldr, we can write this with one traversal
--
-- Define a new version of all that only does a single traversal by
-- "pushing" `p` inside binary operator passed to foldr.
allSingleFold :: (a -> Bool) -> [a] -> Bool
allSingleFold = undefined
