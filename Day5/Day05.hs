import Data.List
import Data.Char
-- import Data.List.Split
import Data.Maybe
-- import Data.Either
-- import Data.Ord(comparing)
-- import Control.Applicative ((<|>))
-- import Text.Parsec.Char(spaces, digit, char,string)
-- import Text.Parsec.String(Parser)
-- import Text.Parsec(parse, many1,ParseError)
-- import qualified Data.Map.Strict as M


readInput :: FilePath -> IO[String]
readInput file = lines <$> readFile file


removeUnit :: String -> String
removeUnit [] =  ""
removeUnit [x] = [x]
removeUnit string@(x:y:xs) | isOppositePolarity x y = removeUnit xs
                           | otherwise = x:removeUnit newString
                        where newString = y:xs
removeAllUnits :: String -> String
removeAllUnits string | hasOppositePolarity string = removeAllUnits newString
                      | otherwise = string
                      where newString = removeUnit string

hasOppositePolarity :: String -> Bool
hasOppositePolarity [] = False
hasOppositePolarity [x,y] = isOppositePolarity x y
hasOppositePolarity (x:y:xs) | isOppositePolarity x y = True
                             | otherwise = hasOppositePolarity (y:xs)

isOppositePolarity :: Char -> Char -> Bool
isOppositePolarity x y |  isUpper x && isLower y = toLower x == y
                       | isLower x && isUpper y = x == toLower y
                       | otherwise = False

-- deleteFromString :: String->Char -> Char -> String
-- deleteFromString string x y = newerString
--     where newString = delete x string
--           newerString = delete y newString



part1 :: FilePath -> IO Int
part1 file = do i <- readInput file
                return $ (length . removeAllUnits . unwords) i


