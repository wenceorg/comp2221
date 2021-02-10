module MergeSort where
import Data.List
-- Split a list into two (approximately) equal-length sublists
-- We can use splitAt and find the midpoint by using div to compute
-- the integer division of the lists length.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Sort a list using merge sort
mergeSort :: Ord a => [a] -> [a]
-- empty lists are sorted
mergeSort [] = []
-- As are singletons (I forget this case first time)
mergeSort [x] = [x]
-- otherwise merge two sorted sublists, which we obtain by splitting
-- the input in halves and recursing on those sublists.
mergeSort xs = merge ys zs
  where (left, right) = halve xs
        ys = mergeSort left
        zs = mergeSort right


-- Now do things using a user-providing ordering function
-- If we check :info Ordering we see that it has three values LT, EQ,
-- and GT.
mergeWith :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
-- We can ignore the ordering function for the empty list cases
mergeWith _ [] ys = ys
mergeWith _ xs [] = xs
-- Now we need it
mergeWith ord (x:xs) (y:ys)
  | ord x y == LT = x : mergeWith ord xs (y:ys) -- x < y, so comes first
  | otherwise     = y : mergeWith ord (x:xs) ys -- otherwise y is first.

mergeSortWith :: (a -> a -> Ordering) -> [a] -> [a]
mergeSortWith _ [] = []
mergeSortWith _ [x] = [x]
-- otherwise merge two sorted sublists, which we obtain by splitting
-- the input in halves and recursing on those sublists.
-- So this is the same as before but we push the extra ordering
-- function through everywhere
mergeSortWith ord xs = mergeWith ord ys zs
  where (left, right) = halve xs
        ys = mergeSortWith ord left
        zs = mergeSortWith ord right

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
-- Using invertOrdering we invert the result of calling and ordering function
invert ord = \x y -> invertOrdering (ord x y)

-- We can actually simplify this to
invert' :: Comparator a -> Comparator a
-- This looks strange because the type signature looks like we only
-- take one argument (but remember the type synonym we introduced
-- above)
invert' ord x y = invertOrdering (ord x y)

-- We can actually do this without invertOrdering at all
invert'' :: Comparator a -> Comparator a
invert'' ord = \x y -> ord y x
-- and, remembering the definition of flip
invert''' :: Comparator a -> Comparator a
invert''' = flip

-- Sorting in reverse is now a case of sorting with an inverted comparator.
sortReverse :: Ord a => [a] -> [a]
sortReverse = mergeSortWith (invert compare)


-- Manipulating comparators

-- Implement the binary function `on` which takes a `Comparator a` and a
-- function (b -> a) to produce a `Comparator b`.
on :: Comparator a -> (b -> a) -> Comparator b
-- We need to return a function which calls our comparator on the
-- transformed arguments
-- Here we use a lambda and call f x and f y to deliver values of type
-- a which our comparator understands
on cmp f = \x y -> cmp (f x) (f y)

-- Now using the invert and on functions, implement sorting a list of
-- pairs in reverse order by the length of its second elements.
sortReversedByLengthSnd :: [(a, String)] -> [(a, String)]
-- For each entry, we need to deliver the length of the second entry
-- Available via f = (length . snd)
-- We (compare `on` f) to implement the comparison, and finally invert
-- that comparator
sortReversedByLengthSnd = mergeSortWith (invert (compare `on` (length . snd)))
