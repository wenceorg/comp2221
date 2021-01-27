module Exercise1 where

-- The badly formatted code
-- N = a 'div' length xs
--     where
--        a = 10
--       xs = [1, 2, 3, 4, 5]
-- Needs a few changes
-- First, identifiers need to start with a lower-case letter, so
-- replace N with n.
-- Second, the way to write infix functions is to use backticks ` (not
-- quotes '), so replace 'div' with `div`.
-- Finally, we need to align the start of the definitions in the where
-- clause (remember the alignment rule)
-- We should also type annotate, since length returns an Int, this
-- value has type Int.
n :: Int
n = a `div` length xs
    where
       a = 10
       xs = [1, 2, 3, 4, 5]

   
-- Here's one way, compute the length of the list, subtract 1 (since
-- everything is zero-indexed) and use the list indexing operator (!!)
-- to get it out.
-- This approach needs to traverse the list twice, once to compute the
-- length and the a second time to index it.
last' :: [a] -> a
last' xs = xs !! (length xs - 1)

-- We can also note that the last element of the list is the first (or
-- head) element of the reversed list. This only traverses the list once.
last'' :: [a] -> a
last'' xs = head (reverse xs)

-- Or, we can write in "pointfree" style, this is the same as the
-- previous definition but now we don't have to explicitly refer to
-- the list xs by name. The (.) operator is function composition.
-- See this discussion for more detail
-- https://github.com/wenceorg/comp2221/discussions/14
last''' :: [a] -> a
last''' = head . reverse

-- To drop the last element (which init does) we can do the same thing
-- as before, figuring out the length of the list and using `take` to
-- take the correct number of items.
-- This approach traverses the list twice
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

-- Again, we can use reverse and tail to do things. This approach now
-- also traverses the list twice.
init'' :: [a] -> [a]
init'' xs = reverse (tail (reverse xs))

-- A pointfree version is now like so.
init''' :: [a] -> [a]
init''' = reverse . tail . reverse

-- If we want to avoid multiple traversals, we have to recurse over
-- the list by hand. The standard library definition does that like so
initOnce :: [a] -> [a]
initOnce [x]                =  []
initOnce (x:xs)             =  x : initOnce xs
initOnce []                 =  undefined


-- To do this, we pattern match and destructure the list into its head
-- and tail (x:xs). Then we move the head to the tail by using (++)
-- "append". You might have tried `xs ++ x` first, which would fail
-- because x :: a, not [a], and (++) :: [a] -> [a] -> [a]. That is,
-- append takes two lists and produces a new one.
shuffle :: [a] -> [a]
shuffle (x:xs) = xs ++ [x]
-- This pattern match is necessary so that shuffle [] produces [].
shuffle [] = []

-- This function implements quicksort (with a rather bad pivot
-- selection).
-- See this discussion
-- https://github.com/wenceorg/comp2221/discussions/7
-- for more details.
mystery [] = []
mystery (x:xs) = mystery ys ++ [x] ++ mystery zs
  where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]

-- If we were to replace a <= x by a < x, then the sorting also
-- removes duplicates.
