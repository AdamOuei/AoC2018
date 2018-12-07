import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
import Control.Applicative ((<|>))
import Text.Parsec.Char(spaces, digit, char,string)
import Text.Parsec.String(Parser)
import Text.Parsec(parse, many1,ParseError)
import qualified Data.Map.Strict as M


type GuardId = Int
data Time = Time {month ,date, hour, minute :: Int} deriving (Eq,Ord,Show)
data Event = NewGuard GuardId | Sleep | Wake deriving (Eq,Ord,Show)
data Entry = Entry {timestamp::Time , event::Event} deriving (Eq,Ord,Show)



-- Part 1 

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseEntry :: Parser Entry
parseEntry = Entry <$> parseTime <* spaces <*> parseEvent

parseEvent:: Parser Event
parseEvent = newGuard <|> sleep <|> wake

newGuard :: Parser Event
newGuard = NewGuard <$> (string "Guard #" *> parseInt <* string " begins shift" )

sleep :: Parser Event
sleep = string "falls asleep" *> pure Sleep

wake :: Parser Event
wake = string "wakes up" *> pure Wake


parseTime :: Parser Time
parseTime = do
        char '['
        parseInt
        char '-'
        month <- parseInt
        char '-'
        day <- parseInt
        spaces
        hour <- parseInt
        char ':'
        minute <- parseInt
        char ']'
        pure $ Time month day hour minute



readInput :: FilePath -> IO[String]
readInput file = lines <$> readFile file

splitAllEntries :: [String] -> [(String,String)]
splitAllEntries = map (splitAt 12 . drop 6) 


splitAllEntries' :: [String] -> [[String]]
splitAllEntries' = map (split (dropBlanks $ dropDelims $ oneOf " #[]") . drop 6)

part1 :: FilePath -> IO (Either ParseError [Entry])--[String]--(String,String)]
part1 file = do i <- readInput file 
                let
                    allEntries = (sortOn fst . splitAllEntries) i
                    otherEntries =  (concat . sort . splitAllEntries') i
                    anotherEntries = traverse (parse parseEntry "stdin") i
                    in
                    return anotherEntries 