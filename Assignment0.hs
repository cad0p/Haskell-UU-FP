module Main where
main :: IO ()
-- 2
-- main = putStrLn "Hello, world!"
-- 3 Interaction with the outside world
main = interact (unlines . map reverse . lines)
