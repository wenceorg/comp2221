module Main where
import Data.Foldable (foldr', foldl')

main :: IO ()
main = do
  let xs = replicate (10^7 :: Int) (1 :: Int)
  print "Computing last"
  print $ {-# SCC last_xs #-} last xs
  print "Computing length"
  print $ {-# SCC length_xs #-} length xs
  print "lazy foldr"
  print $ {-# SCC foldr_lazy #-} foldr (+) 0 xs
  print "strict foldr'"
  print $ {-# SCC foldr_strict #-} foldr' (+) 0 xs
  print "lazy foldl"
  print $ {-# SCC foldl_lazy #-} foldl (+) 0 xs
  print "strict foldl"
  print $ {-# SCC foldl_strict #-} foldl' (+) 0 xs  
