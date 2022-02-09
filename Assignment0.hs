module Main where
import Data.List (intercalate)
main :: IO ()
-- 2
-- main = putStrLn "Hello, world!"
-- 3 Interaction with the outside world
-- main = interact (unlines . map reverse . lines)
-- 4 The exercise
main = interact work
  where work = intercalate " / " . map reverse . lines