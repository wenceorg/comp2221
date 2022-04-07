module Exercise3 where

-- compress should eliminate consecutive duplicate elements of a list
-- For example, compress [1, 3, 1, 1, 2, 5] == [1, 3, 1, 2, 5]
-- Question: Why do we need the "Eq a" class constraint?
compress :: Eq a => [a] -> [a]
compress = undefined

-- A pythagorean triplet is a triple of positive integers (x, y, z)
-- such that x^2 + y^2 = z^2.
-- Define a function pyths using a list comprehension that returns all
-- pythagorean triples with components in [1..n], don't wory about
-- duplicates.
-- For example
-- pyths 5 == [(3, 4, 5), (4, 3, 5)]
-- Hint: Integer exponentiation is write x^y
pyths :: Int -> [(Int, Int, Int)]
pyths = undefined


-- Now do the same thing, but modify your solution to only produce
-- unique triples.
-- So for example
-- pyths' 5 == [(3, 4, 5)]
-- or some permutation (but there should only be one entry in the
-- list)
-- Hint: remember that in a list comprehension assignments can refer
-- to variables that are already bound.
pyths' :: Int -> [(Int, Int, Int)]
pyths' = undefined

-- The scalar product of two vectors (lists) x, and y, of numbers of
-- length $n$ is sum_i x_i y_y.
-- Compute the scalar product using a list comprehension
scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct = undefined

-- Now let's do the same thing using zip, map, and sum
-- Use zip to merge the two lists into tuples, do the pointwise
-- multiplication with map and then sum up the results.
scalarproduct' :: Num a => [a] -> [a] -> a
scalarproduct' = undefined

-- Finally, we'll note that the pattern of mapping a binary function
-- over two zipped lists is captured by the library function zipWith.
-- So try doing the same thing using sum and zipWith
scalarproduct'' :: Num a => [a] -> [a] -> a
scalarproduct'' = undefined
