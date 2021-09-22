module Lec10 where
import Prelude hiding (return, lookup)
-- Catching failures

-- Remember the Maybe type
-- Suppose we have

-- Let's make things clearer writing

failure :: Maybe a
failure = Nothing

return :: a -> Maybe a
return x = Just x

lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ [] = failure
lookup k ((k', v'):kvs) =
  if k == k'
  then return v'
  else lookup k kvs

data Tree a =
  Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

replaceAll :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
replaceAll kvs Leaf = return Leaf
replaceAll kvs (Node l k r) =
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
-- Pattern
continue :: Maybe a -> (a -> Maybe b) -> Maybe b
continue op k = case op of
                  Nothing -> Nothing
                  Just a -> k a

replaceAll_v2 :: Eq k => [(k, v)] -> Tree k -> Maybe (Tree v)
replaceAll_v2 kvs Leaf = return Leaf
replaceAll_v2 kvs (Node l k r) =
  continue (replaceAll_v2 kvs l) (\l' ->
  continue (lookup k kvs)        (\v ->
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
-- `continue` pattern in a principled way. So we can use do notation
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
