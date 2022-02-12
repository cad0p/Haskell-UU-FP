-- Assignment 1: Lists
module Main where

import           Data.Char                      ( isDigit )
import           Data.List                      ( intercalate, transpose )

main :: IO ()

type Field = String

type Row = [Field]

type Table = [Row]

parseTable :: String -> Table
parseTable = map words . lines

type Width = Int

replicateL :: Width -> String
replicateL x = replicate x '-'

-- | The 'printLine' function prints a line
printLine :: [Width] -> String
printLine = wrapPlus . intercalate "+" . map replicateL
  where wrapPlus x = concat ["+", x, "+"]

{-|
  given a desired width for a field and the contents of a field, 
  returns a formatted field by adding additional whitespace. 
  If the field only consists of numerical digits, 
  the field should be right-aligned, 
  otherwise it should be left-aligned.
-}
printField
  :: Width      -- ^ desired width for the field
  -> String   -- ^ contents of the field
  -> String   -- ^ formatted field
printField w c = formatField (isFieldNumerical c) w c
 where
  isFieldNumerical = all isDigit
  
formatField :: Bool -> Width -> String -> Field
-- left aligned
formatField False w c = concat (c : replicate (w - length c) " ")
-- right aligned
formatField True  w c = concat (replicate (w - length c) " " ++ [c])

{-|
  Write a function printRow :: [ (Int, String)] → String 
  that, given a list of pairs
  —the left element giving the desired length of a field 
  and the right element its contents—
  formats one row in the table. For example,
  printRow [(5, "Alice"),(6, "Allen"),(6, "female"),(6, "82000")]
  should return the formatted row
  "|Alice|Allen |female| 82000|"
-}
printRow :: [(Width, String)] -> String 
printRow = intercalate "|" . map (uncurry printField)

{-|
  given a table, computes the necessary widths
  of all the columns
-}
columnWidths :: Table -> [Width]
-- columns become rows; for each row, get the length of each field
-- and get the maximum of these lengths
columnWidths = map (maximum . map length) . transpose


main = interact (unlines . map show . columnWidths . parseTable)
