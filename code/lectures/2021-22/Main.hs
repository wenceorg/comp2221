module Main where

import Lec09 (foldl, foldl', foldr)
import System.TimeIt (timeIt)
import Prelude hiding (foldl, foldl', foldr)

-- Strict vs lazy

-- If we compile with -O2, then GHC's strictness analysis is able to
-- determine that the folded function can always be applied strictly
-- (and so foldl and foldl' behave identically).
-- To see a difference we must compile with -O0 (or -O1 for me).
-- See https://wiki.haskell.org/Performance/Strictness for more details.
main :: IO ()
main = do
  let xs = [1 .. 10 ^ 7]
  print $ length xs
  timeIt $ putStrLn ("foldl: " ++ show (foldl (\acc x -> (x * 7) + acc) 0 xs))
  timeIt $ putStrLn ("foldl': " ++ show (foldl' (\acc x -> (x * 5) + acc) 0 xs))
  timeIt $ putStrLn ("foldr: " ++ show (foldr (\x acc -> (x * 9) + acc) 0 xs))
