module Lec09 where
import Prelude hiding (foldr, foldl, ($!))

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = x `f` foldr f z xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x : xs) = foldl f (f z x) xs


($!) :: (a -> b) -> a -> b
-- Force x to WHNF then evaluate f x, and return that
f $! x = x `seq` f x


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
-- Want to reduce (f z x) before recursing, so use $!.
-- This evaluates (f z x) to WHNF and then applies the reduction rule
-- for foldl'
foldl' f z (x : xs) = (foldl' f $! f z x) xs

