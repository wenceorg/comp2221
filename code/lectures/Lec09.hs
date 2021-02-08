module Lec09 where
import Data.IORef (IORef, newIORef, readIORef,
                   writeIORef, modifyIORef)
-- What's the problem?

putc :: Char -> ()
putc = undefined

expr = (putc 'x', putc 'x')
x = let x = putc 'x' in (x, x)

-- Haskell solves this by wrapping the printing in an IO action
-- :type putChar

-- Conceptually, but not implemented like this
data World = Everything
type MyIO a = World -> (a, World)

-- Interface pure Haskell with these effectful actions through the
-- Monad interface. Which we will use today, and explain a bit more of
-- next time.

-- Creating an action
-- Need to lift into impure world
-- And execute them

put2 :: Char -> Char -> IO ()
put2 x y = do
  return ()


printLine :: String -> IO ()
printLine xs =
  do mapM_ putChar xs
     putChar '\n'

-- getting things out
get2 :: IO (Char, Char)
get2 = do
  x <- getChar
  y <- getChar
  return (x, y)
  

readLine :: IO String
readLine =
  do c <- getChar
     if c == '\n'
     then return ""
     else do cs <- readLine
             return (c:cs)

