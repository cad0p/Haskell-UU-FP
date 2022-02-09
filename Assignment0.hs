module Main where
main :: IO ()
-- 2
-- main = putStrLn "Hello, world!"
-- 3 Interaction with the outside world
main = interact work
  where work text = unlines (map reverse (lines text))
