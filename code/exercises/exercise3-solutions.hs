{-# LANGUAGE ParallelListComp #-}
module Exercise3 where

-- compress should eliminate consecutive duplicate elements of a list
-- For example, compress [1, 3, 1, 1, 2, 5] == [1, 3, 1, 2, 5]
-- Question: Why do we need the "Eq a" class constraint?
-- We're going to have to compare elements so we can tell which ones to drop.
-- This requires that a satisfies the Eq typeclass interface.
compress :: Eq a => [a] -> [a]
-- Empty list, nothing to do
compress [] = []
-- If you wanted to match the pattern and bind the whole thing to a
-- variable, you would do
-- compress xs@[_] = xs
compress [x] = [x]
-- We have at least two elements, great, there's something we might be
-- able to compress
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = x : compress (y:xs)

-- It's interesting to see how to write this with foldr (see also
-- exercise 5).
-- The selection function is basically the same
-- but the second argument which is now the tail of the list
-- is already transformed.
-- So if we reach the end, then we return our current entry
-- otherwise we just check if we should cons our current entry on the
-- front of the already-transformed tail.
compress' :: Eq a => [a] -> [a]
compress' = foldr pick []
  where pick x [] = [x]
        pick x (y:xs)
          | x == y    = y:xs
          | otherwise = x:y:xs

-- A pythagorean triplet is a triple of positive integers (x, y, z)
-- such that x^2 + y^2 = z^2.
-- Define a function pyths using a list comprehension that returns all
-- pythagorean triples with components in [1..n], don't wory about
-- duplicates.
-- For example
-- pyths 5 == [(3, 4, 5), (4, 3, 5)]
-- Hint: Integer exponentiation is written x^y
pyths :: Int -> [(Int, Int, Int)]
-- We just iterate over all tuples of integers are keep those that
-- satisfy the relation we want.
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]


-- Now do the same thing, but modify your solution to only produce
-- unique triples.
-- So for example
-- pyths' 5 == [(3, 4, 5)]
-- or some permutation (but there should only be one entry in the
-- list)
-- Hint: remember that in a list comprehension assignments can refer
-- to variables that are already bound.
pyths' :: Int -> [(Int, Int, Int)]
-- This time we start with z and for the later generators only count
-- up as far as the previous value.
-- Aside: this style of formatting is quite typical (the commas at the
-- beginning of the line allow us to align the generators so we can
-- see the patterns).
pyths' n = [(x, y, z)
           | z <- [1..n]
           , y <- [1..z]
           , x <- [1..y]
           , x^2 + y^2 == z^2]

-- The scalar product of two vectors (lists) x, and y, of numbers of
-- length $n$ is sum_i x_i y_y.
-- Compute the scalar product using a list comprehension
scalarproduct :: Num a => [a] -> [a] -> a
-- Here's one way, really we're kind of cheating because we're zipping
-- too.
-- To be able to do this without zip we need to turn on the
-- ParallelLispComp LANGUAGE extension
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
-- scalarproduct xs ys = sum [x * y | x <- xs | y <- ys]

-- Now let's do the same thing using zip, map, and sum
-- Use zip to merge the two lists into tuples, do the pointwise
-- multiplication with map and then sum up the results.
scalarproduct' :: Num a => [a] -> [a] -> a
-- zip delivers a pair (x, y) and (*) is in curried form, so we
-- uncurry it to use it in map.
scalarproduct' xs ys = sum (map (uncurry (*)) (zip xs ys))

-- Finally, we'll note that the pattern of mapping a binary function
-- over two zipped lists is captured by the library function zipWith.
-- So try doing the same thing using sum and zipWith
-- In fact, the LSP interaction in my editor underlines the map
-- definition above and suggests that I use zipWith instead
scalarproduct'' :: Num a => [a] -> [a] -> a
scalarproduct'' xs ys = sum (zipWith (*) xs ys)
