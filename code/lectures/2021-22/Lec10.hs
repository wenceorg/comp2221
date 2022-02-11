module Lec10 where
import Data.Foldable (for_, foldl')
import Prelude hiding (putStr, putStrLn, lookup)
import Control.Monad (mapM_)
-- What's the problem?
putc :: Char -> ()
putc c = undefined

-- Referential transparency means that the transformation from the
-- definition of expr into that of expr' must be safe. But now we only
-- evaluate the put once.
expr :: ((), ())
expr = (putc 'x', putc 'x')

expr' = let printed = putc 'x' in (printed, printed)
-- Similarly, imagine

getc :: () -> Char
getc () = undefined


-- Haskell solves this by wrapping the printing in an IO action
-- :type putChar
-- Conceptually, but not implemented like this
data World = Everything
type MyIO a = World -> (a, World)

-- The type is actually IO a

-- Interface pure Haskell with these effectful actions through the
-- Monad interface. Here we will basically just use do notation.


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
  do
    putChar x
    putStr xs
  -- -- print the head of the list (inside do, which chains a sequence of
  -- -- actions together).
  -- do putChar x
  -- -- Recurse on the tail.
  --    putStr xs

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
  _ <- getChar
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

-- This same idea also applies to other types, here we see Maybe again:
-- Catching failures

-- Remember the Maybe type
-- Let's make things clearer writing

failure :: Maybe a
failure = Nothing

-- Lookup a key in a list of bindings and maybe produce a value
lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ [] = failure
lookup k ((k', v'):kvs) =
  if k == k'
  -- For Maybe, return == Just
  then return v'
  else lookup k kvs

data Tree a =
  Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

-- Suppose we want to push our lookup into a tree
-- turning a Tree of keys into Maybe a Tree of values
-- As soon as we fail in the lookup we want to bail out and return Nothing
replaceAll :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
replaceAll kvs Leaf = return Leaf
replaceAll kvs (Node l k r) =
  -- Can do this by nested case analysis, but it is hard to see the
  -- structure of what is going on.
  case replaceAll kvs l of
    Nothing -> failure
    Just l' ->
      case lookup k kvs of
        Nothing -> failure
        Just v ->
          case replaceAll kvs r of
            Nothing -> failure
            Just r' -> return (Node l' v r')

-- Ugh, this is hard to follow
-- The pattern is that we have a function "continuation" that runs if
-- the operands is not Nothing, by applying the continuation to the
-- unwrapped operand.
continue :: Maybe a -> (a -> Maybe b) -> Maybe b
continue Nothing continuation = Nothing
continue (Just a) continuation = continuation a
-- Note the similarity with fmap :: (a -> b) -> Maybe a -> Maybe b
-- Here instead we have continue :: Maybe a -> (a -> Maybe b) -> Maybe b
-- So the input is unwrapped before the function is applied.

replaceAll_v2 :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
replaceAll_v2 kvs Leaf = return Leaf
replaceAll_v2 kvs (Node l k r) =
  -- Now we can replace our nested case analysis
  -- with lookup + continuation.
  continue (replaceAll_v2 kvs l) (\l' ->
  continue (lookup k kvs)        (\v  ->
  continue (replaceAll_v2 kvs r) (\r' ->
  return (Node l' v r'))))

-- Or using infix notation

replaceAll_v3 :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
replaceAll_v3 kvs Leaf = return Leaf
replaceAll_v3 kvs (Node l k r) =
  replaceAll_v3 kvs l `continue` \l' ->
  lookup k kvs        `continue` \v ->
  replaceAll_v3 kvs r `continue` \r' ->
  return (Node l' v r')

-- Maybe implements the Monad typeclass, which captures this
-- `continue` pattern in a principled way.
-- It is called (>>=), pronounced "bind"
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:-62--62--61-
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- This is what do-notation de-sugars to.
-- as >>= bs
-- is equivalent to
-- do a <- as (unwrap the as action)
--    bs a    (feed it into bs action)
-- So we can use do notation
-- to unwrap Maybes and chain computation, bailing out when we fail.
replaceAll_v4 :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
replaceAll_v4 kvs Leaf = return Leaf
replaceAll_v4 kvs (Node l k r) =
  do l' <- replaceAll_v4 kvs l
     v  <- lookup k kvs
     r' <- replaceAll_v4 kvs r
     return (Node l' v r')

-- So continue gives us a way of chaining computations that may fail
-- and bailing out if one of them does.
tree :: Tree Char
tree = Node (Node Leaf 'a' Leaf) 'b' (Node (Node Leaf 'c' Leaf) 'd' Leaf)

bindings :: [(Char, Int)]
bindings = zip "abcd" [1..]

treeRewritten :: Maybe (Tree Int)
treeRewritten = replaceAll_v3 bindings tree
