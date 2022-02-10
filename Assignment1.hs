-- Assignment 1: Lists
module Main where

import           Data.Array
import           Data.List

main :: IO ()

type Field = String

type Row = [Field]

type Table = [Row]

parseTable :: String -> Table
parseTable = map words . lines

type Width = Int

replicateL :: Int -> String
replicateL x = replicate x '-'

printLine :: [Width] -> String 
printLine = unlines . map replicateL

main = putStrLn(printLine [10, 10, 10, 10])
