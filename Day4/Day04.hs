import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
import Data.Ord(comparing)
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

getEntries :: [String] -> Either ParseError [Entry]
getEntries  = traverse (parse parseEntry "stdin") 

sleepTimes :: [Entry] -> [(GuardId,Time)]
sleepTimes = help 0 
        where help _ [] = []
              help _ (Entry _ (NewGuard id ): xs) = help id xs
              help gid (Entry start Sleep : Entry end Wake:xs) = 
                let (Time mon d h _) = start
                    more = help gid xs
                in  [(gid , Time mon d h t) | t <- [minute start.. minute end -1 ]] ++ more
 
helper :: [Entry] -> (GuardId,Int)
helper entries = let minutes = fmap (fmap minute) (sleepTimes entries) 
                     mostSleept = frequencies (map fst minutes)
                     (id,_) = mostCommon mostSleept
                     minuteSlept = frequencies . map snd . filter ((==id ). fst) $ minutes
                     (min,_) = mostCommon minuteSlept
                 in  (id , min)

                 where mostCommon   = maximumBy (comparing snd) . M.toList


frequencies :: Ord k => [k] -> M.Map k Int
frequencies = foldl' (\m x-> M.insertWith (+) x 1 m ) M.empty

splitAllEntries' :: [String] -> [[String]]
splitAllEntries' = map (split (dropBlanks $ dropDelims $ oneOf " #[]") . drop 6)

part1 :: FilePath -> IO (GuardId,Int)
part1 file = do i <- readInput file
                let sortedInput = sort i
                    entries = fromRight [] $ getEntries sortedInput 
                    answer = helper entries
                    in return answer