module MergeSort where
import Data.List
-- Split a list into two (approximately) equal-length sublists
halve :: [a] -> ([a], [a])
halve = undefined

-- Merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined

-- Sort a list using merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined


-- Now do things using a user-providing ordering function
mergeWith :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeWith = undefined

mergeSortWith :: (a -> a -> Ordering) -> [a] -> [a]
mergeSortWith = undefined

-- You can now get the implementation with the Ord class constraint by
-- using the builtin generic function `compare` as the comparator

mergeSort' :: Ord a => [a] -> [a]
mergeSort' = mergeSortWith compare

-- A type synonym to save on typing
type Comparator a = a -> a -> Ordering


-- Manipulation of comparisons
-- Swap the order of a comparison
invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering GT = LT
invertOrdering EQ = EQ

-- Implement the inverse of a comparator by using invertOrdering
invert :: Comparator a -> Comparator a
invert = undefined

sortReverse :: Ord a => [a] -> [a]
sortReverse xs = mergeSortWith (invert compare) xs


-- Manipulating comparators

-- Implement the binary function `on` which takes a `Comparator a` and a
-- function (b -> a) to produce a `Comparator b`.
on :: Comparator a -> (b -> a) -> Comparator b
on = undefined

-- Now using the invert and on functions, implement sorting a list of
-- pairs in reverse order by the length of its second elements.
sortReversedByLengthSnd :: [(a, String)] -> [(a, String)]
sortReversedByLengthSnd = undefined
