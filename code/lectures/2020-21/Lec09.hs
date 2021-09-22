module Lec09 where
import Data.Foldable (for_, foldl')
import Prelude hiding (putStr, putStrLn)

-- What's the problem?
putc :: Char -> ()
putc = undefined

-- Referential transparency means that the transformation from the
-- definition of expr into that of expr' must be safe. But now we only
-- evaluate the put once.
expr = (putc 'x', putc 'x')
expr' = let x = putc 'x' in (x, x)

-- Haskell solves this by wrapping the printing in an IO action
-- :type putChar
-- Conceptually, but not implemented like this
data World = Everything
type MyIO a = World -> (a, World)

-- Interface pure Haskell with these effectful actions through the
-- Monad interface. In this course we will basically just use do notation

-- Need to lift into impure world
put2 :: Char -> Char -> IO ()
put2 x y = do
  -- Now we are in "action" land
  putChar x
  putChar y
  -- Return an empty result, indicated by the empty tuple.
  return ()

-- We can do the same stuff we saw with recursion previously
-- But now we must again be sure to keep things in action world.
putStr :: String -> IO ()
-- Base case lift () into IO.
putStr [] = return ()
putStr (x:xs) =
  -- print the head of the list (inside do, which chains a sequence of
  -- actions together).
  do putChar x
  -- Recurse on the tail.
     putStr xs

-- Or using `for_`
putStr' :: String -> IO ()
-- This (imported from Data.Foldable) gives us side-effectful
-- iteration over a Foldable type. Here we visit every entry in the
-- list calling putChar.
putStr' xs = for_ xs putChar

-- Again, we can chain actions together
putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

-- getting things out works the same way, but now our action wraps a
-- type with values.
get2 :: IO (Bool, Char)
get2 = do
  -- Inside do, we can unwrap the return value, x is now pure and is
  -- the result of executing the action
  x <- getChar
  -- We'll ignore the next character
  getChar
  -- And put the final one inside y
  y <- getChar
  -- Now we can call pure functions on x and y, here we ask if they
  -- are equal.
  -- So (x == y, y) :: (Bool, Char), and we use return to lift into IO land.
  return (x == y, y)

-- A little more.
readLine :: IO String
readLine =
  do c <- getChar
     if c == '\n'
     then return ""
     else do cs <- readLine
             return (c:cs)

-- The major conceptual hurdle is that once we're in action space, we
-- can't get out again. This might make us think that basically all of
-- our program must be impure. However, usually the interaction can be
-- isolated to a few places inside of which we can maintain purity.
-- The idea of IO actions also allows us to do the normal Haskell
-- thing of equational substitution, it's just that we're not
-- substituting values but "plans to create values".
