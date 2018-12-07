import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
import Text.Parsec.Char(spaces, digit, char)
import Text.Parsec.String(Parser)
import Text.Parsec(parse, many1,ParseError)
import qualified Data.Map.Strict as M



data Guard = Guard { guardId:: Int, actions:: [(String,String)] }
    deriving(Show,Eq)



-- Part 1 

readInput :: FilePath -> IO[String]
readInput file = lines <$> readFile file

splitAllEntries :: [String] -> [(String,String)]
splitAllEntries = map (splitAt 12 . drop 6) 


splitAllEntries' :: [String] -> [[String]]
splitAllEntries' = map (split (dropBlanks $ dropDelims $ oneOf " #[]") . drop 6)

part1 :: FilePath -> IO [[String]]--(String,String)]
part1 file = do i <- readInput file 
                let
                    allEntries = (sortOn fst . splitAllEntries) i
                    otherEntries = (sort .splitAllEntries') i
                    in
                    return otherEntries 