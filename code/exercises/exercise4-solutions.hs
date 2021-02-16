{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module Exercise04 where
-- A warmup, generating prime numbers
candidates :: [Integer]
candidates = [2..]

sieve :: [Integer] -> [Integer]
sieve [] = []
-- The first entry in the list must be prime, then we recurse on the
-- tail, filtering out all those elements which are divisible by our
-- prime.
-- This is actually doing "trial division" rather than the true sieve
-- of Eratosthenes.
sieve (x:xs) = x : sieve [x' | x' <- xs, x' `mod` x /= 0]

primes :: [Integer]
primes = sieve candidates

-- Now we'll do a better job of generating Pythagorean triples than
-- last time.
mult :: Integral a => [[a]] -> [a] -> [a]
-- multiply the rows of the matrix onto the single "column" vector.
mult mat xcol = map dot mat
  where dot row = sum (zipWith (*) row xcol)

-- This is where all the work happens!
-- This is defining a branching tree with 3 branches at each level.
treeGen :: Integral a => [[a]] -> [[a]]
treeGen mns = heads ++ treeGen heads
  where
    -- For each mn we produce the branches, and then concatenate them
    -- all together.
    heads = concatMap next mns
    -- compute the branches from a single mn pair
    -- e.g. the first level of branching takes as input [2, 1]
    --  map ((flip mult) [2, 1]) generators
    -- == [[3,2],[5,2],[4,1]]
    -- We then need to recurse on each of these
    next mn = map ((flip mult) mn) generators
    -- These are the three generating matrices.
    generators = [[[2, -1], [1, 0]],
                  [[2, 1], [1, 0]],
                  [[1, 2], [0, 1]]]

-- A type synonym
type Triple a = (a, a, a)
-- Just convert from our pairs to triples
triplify :: Integral a => [a] -> Triple a
triplify [m, n] = (m^2 - n^2, 2*m*n, m^2 + n^2)
-- Since we're using lists to represent the pairs, our function isn't
-- total, but should never be called like this, so use undefined.
-- Perhaps better would be to have a Pair a type, but that makes the
-- implementation of mult somewhat uglier.
-- Exercise: try redoing everything with
-- type Pair a = (a, a)
-- type Mat a = ((a, a), (a, a))
triplify _      = undefined

-- Compute all triples by mapping over the list of pairs.
primitiveTriples :: Integral a => [Triple a]
primitiveTriples =
  let mn = [2, 1] in map triplify (mn : treeGen [mn])

-- Extension exercise: using lists to represent a tree with 3-way
-- branching is not very type safe. Try redoing everything with
type Pair a = (a, a)
data Tree a = Leaf | Tree (Pair a) (Tree a) (Tree a) (Tree a)
  deriving (Eq, Show, Functor, Foldable)
-- Was that better, easier, harder?
