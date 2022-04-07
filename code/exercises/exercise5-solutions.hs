module Exercise5 where

-- Linear recursive functions with folds

-- With foldr
-- We saw this a few times in the lectures
length' :: [a] -> Int
length' = foldr (const (1+)) 0

-- Remember that foldr (:) [] == id
-- So here what we want to do is transform the first argument with the
-- mapping function f and then cons it on.
-- If we desugar the function we have
-- (:) . f == \x -> (:) (f x) == \x -> \xs -> f x : xs
-- And resugaring a bit \x xs -> f x : xs
-- Which is exactly the lambda we want for walking down a list
-- applying f to each element in a foldr.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- xs is the already reversed tail, so we append the single element
-- list containing the head.
-- Notice that this is quadratic in the length of the list since each
-- append must walk down to the end.
-- If you need to reverse the structure in a fold, prefer to use foldl instead.
reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

-- Combine with (&&) and an initial value of True
and' :: [Bool] -> Bool
and' = foldr (&&) True

-- And with foldl
-- This one is straightforward except that the function has type
-- b -> a -> b (so we need to `flip` (const (1+)) compared to the
-- foldr case).
length'' :: [a] -> Int
length'' = foldl (flip (const (1+))) 0

-- Remember that foldl reverses the structure, so we have to undo that
-- in the map. So like the foldr for reverse case above, this is
-- expensive since it is quadratic in the length of the list (append
-- has to walk to the end every time).
-- If you don't want to reverse the structure, prefer foldr over foldl.
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldl (\xs x -> xs ++ [f x]) []

-- Reversing, however, is straightforward (we saw this in lectures too).
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- This one is fine since all the arguments are the same
and'' :: [Bool] -> Bool
and'' = foldl (&&) True

-- Now some more higher-order functions.

-- Every entry satisfies a predicate
all' :: (a -> Bool) -> [a] -> Bool
-- Here I write in partially "pointfree" manner, not referring to the
-- list we're going to map over.
-- This is effectively equivalent to
-- all' p xs = and (map p xs)
-- Aside, to write things totally pointfree I would have
-- all' = (and .) . map
-- Do you understand why? Try desugaring the compositions.
all' p = and . (map p)

-- At least one entry satisfies a predicate
any' :: (a -> Bool) -> [a] -> Bool
-- This is the same as all', but we combine with or, rather than and.
any' p = or . (map p)

-- Return elements from a list while they satisfy a predicate, and
-- stop as soon as the predicate is not satisfied
-- The idea is to do the recursion and short-circuit (by returning an
-- empty list) as soon as the predicate is not fulfilled.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x:xs else []) []

-- Remove elements from a list while they satisfy some predicate, and
-- then return the remainder. This one is MUCH HARDER, if you want to
-- have a go, follow along with this paper
-- https://wiki.haskell.org/wikiupload/1/14/TMR-Issue6.pdf
-- You would probably write this one recursively because it's much easier.
dropWhile' :: (a -> Bool) -> [a] -> [a]
-- After http://www.cs.nott.ac.uk/~pszgmh/fold.pdf, which see for more
-- exposition.
dropWhile' p = fst . foldr f ([],[]) where
  f e (xs,ys) = (if p e then xs else zs, zs) where zs = e : ys

-- non-strict (does work on infinite lists)
-- builds up (id . tail ... tail) for as many times as p holds
-- After https://wiki.haskell.org/wikiupload/1/14/TMR-Issue6.pdf
-- The foldr (parenthesised for clarity) walks a list and builds a
-- function that will, when applied to the same list, return the
-- correct result.
-- Example, if p = (< 2) and xs = [1, 1, 1, 2, 1, 3]
-- Then (foldr combine id xs) ==
-- \xs -> id (tail (tail (tail xs)))
-- When applied to xs, this function returns [2, 1, 3]
-- The above-linked PDF provides more explanation. 
dropWhile'' p xs = (foldr combine id xs) xs
  where combine next recurse list@(_:xs)
          | p next = recurse xs
          | otherwise = list

-- You might have implemented all' with
-- all' p xs = and (map p xs)
-- This approach traverses the list twice, but noting the definition
-- of map using foldr, we can write this with one traversal
--
-- Define a new version of all that only does a single traversal by
-- "pushing" `p` inside binary operator passed to foldr.
-- If we wrote
-- all p xs = and (map p xs)
-- Then let's expand the and into a foldr
-- all p xs = foldr (&&) True (map p xs)
-- The foldmap fusion law is
-- foldr op z (map p xs) == foldr (\x ys -> p x `op` ys) z xs
-- In this case op = (&&), z = True. So we arrive at
-- foldr (\x xs -> p x && xs) True
-- Which we can pointfree-ise into ((&&) . p)
-- This is a nice example of the kind of transformations referential
-- transparency and equational reasoning buys us. We don't have to
-- worry that map is going to do some side-effect that would make the
-- transformation invalid.
allSingleFold :: (a -> Bool) -> [a] -> Bool
allSingleFold p = foldr ((&&) . p) True
