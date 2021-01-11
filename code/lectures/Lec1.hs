module Lec1 where
-- Import the standard library (this is done by default)
-- But don't import the filter function (we want to implement it
-- ourselves)
import Prelude hiding (filter)

-- A list of integers
xs :: [Int]
xs = [11, 3, 25, -1, 7]

-- A filtered list, what is strange about this definition?
filtered :: [Int]
filtered = filter (<10) xs

{- Now we have a multi-line comment.

 A filter function, we're replacing the standard library definition,
 which is why we hid it on import above.
 We'll implement it now
-}
filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

-- If we want to run as a program we need to define a main function.
main :: IO ()
main = putStrLn (show filtered)
