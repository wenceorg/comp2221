module Exercise2 where

-- last' should return the final element of a non-empty list
-- Try implementing it using builtin functions (excluding last!) that
-- you already know about
last' :: [a] -> a
-- undefined has type a, so it always type-checks, but indicates that
-- we haven't implemented this function.
last' = undefined

-- Now try using recursion and pattern matching
last'' :: [a] -> a
last'' = undefined

-- tail returns the tail of a list.
-- For example
-- tail [1, 2, 3] == [2, 3]
-- If applied to an empty list, it raises an exception

-- Write safetail, which should map the empty list to itself
-- First using a conditional expression
safetail :: [a] -> [a]
safetail = undefined

-- Now using guard expressions
safetail' :: [a] -> [a]
safetail' = undefined

-- And finally using pattern matching
safetail'' :: [a] -> [a]
safetail'' = undefined

-- Note that the right way to do this is using the Maybe datatype,
-- we'll see that later.


