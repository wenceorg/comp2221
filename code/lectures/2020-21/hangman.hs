module Main where
import System.IO (
  -- Import some functions that prod the IO system
  hSetBuffering, hSetEcho, stdout, stdin,
  -- This is how to import a data type and a
  -- particular inhabitant of a data type
  -- If you want all of them, use BufferMode(..)
  BufferMode(NoBuffering))

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hangman

hangman :: IO ()
hangman =
  do putStrLn "Think of a word: "
     word <- getLineSecretly
     play word

getLineSecretly :: IO String
getLineSecretly =
  do hSetEcho stdin False
     xs <- getLine
     putStrLn (replicate (length xs) '_')
     hSetEcho stdin True
     return xs


play :: String -> IO ()
play word =
  do putStr "What is your guess? "
     guess <- getLine
     if guess == word
     then putStrLn "Correct!"
     else do
         putStrLn (getMatches word guess)
         play word

getMatches :: String -> String -> String
getMatches xs ys = [if x `elem` ys then x else '_' | x <- xs]
