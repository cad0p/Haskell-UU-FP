-- Assignment 1: Lists
module Main where
import           Data.List                      ( intercalate )


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
printLine = wrapPlus . intercalate "+" . map replicateL
  where wrapPlus x = concat ["+", x, "+"]

main = putStrLn (printLine [5, 6, 6, 6])
