{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Exercise06 where
import Data.Foldable -- To make toList available
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap = undefined

instance Foldable Tree where
  foldr = undefined

toTree :: [a] -> Tree a
toTree = undefined

depth :: Tree a -> Int
depth = undefined

balanced :: Tree a -> Bool
balanced = undefined

symmetric :: Tree a -> Bool
symmetric = undefined

-- You may find this useful for symmetric
mirror :: Tree a -> Tree a -> Bool
mirror = undefined


data Expr = Val Int | Add Expr Expr
  deriving (Eq, Show)

expr = Add (Val 1) (Add (Val 4) (Val 5))

eval :: Expr -> Int
eval = undefined

collect :: Expr -> [Expr]
collect = undefined

size :: Expr -> Int
size = undefined


folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde = undefined

eval' :: Expr -> Int
eval' = folde id (+)

size' :: Expr -> Int
size' = undefined

collect' :: Expr -> [Expr]
collect' = undefined
