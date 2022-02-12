-- Assignment 1: Lists
module Main where

import           Data.Char                      ( isDigit, toUpper )
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
  :: Width    -- ^ desired width for the field
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
printRow :: [(Width, Field)] -> String 
printRow = wrapWith "|" . intercalate "|" . map (uncurry printField)

{-|
  given a table, computes the necessary widths
  of all the columns
-}
columnWidths :: Table -> [Width]
-- columns become rows; for each row, get the length of each field
-- and get the maximum of these lengths
columnWidths = map (maximum . map length) . transpose

type Index = Int

{-|
  pretty prints the whole table as a list of lines
-}
printTable :: Table -> [String]
-- printTable t = printHeader(columnWidths t) ++ printRows(columnWidths t)
--   where printHeader x = [ 
--           printLine x, 
--           map toUpper(printRow(zip (columnWidths t) (head t))), 
--           printLine x ]
--         printRows x = [printLine x]
-- you have a list of lists, so you concat to get a list
-- because each row can generate multiple prettified rows
printTable t = concat (mapInd prettyPrintRow t)
  where ws = columnWidths t
        lt = length t
        prettyPrintRow :: Index -> Row -> [String]
        -- header row
        prettyPrintRow 0 r = [
          printLine ws,
          map toUpper(printRow(zip ws r)),
          printLine ws]
        -- /= means not!
        prettyPrintRow i r = filter (/= "") [
          printRow(zip ws r),
          if i == lt - 1 then printLine ws else ""]

-- variant of map that passes each element's index as the first argument of f
-- https://stackoverflow.com/questions/16191824/index-of-element-in-list-in-haskell
mapInd :: (Index -> a -> b) -> [a] -> [b]
mapInd f = zipWith f [0..]

wrapWith :: 
     String -- ^ the wrap
  -> String -- ^ the string
  -> String -- ^ the wrapped string
wrapWith w s = w ++ s ++ w


main = interact (unlines . printTable . parseTable)
