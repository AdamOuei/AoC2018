import Text.Parsec.Char(spaces, digit, char,string)
import Text.Parsec.String(Parser)
import Text.Parsec(parse, many1,ParseError)
import Data.Either

type Pos = (Int,Int)
readInput :: FilePath -> IO [String]
readInput file = lines <$> readFile file


parseInt :: Parser Int
parseInt = read <$> many1 digit

parsePositions :: Parser Pos
parsePositions= do
        x <- parseInt
        char ','
        spaces
        y <- parseInt
        pure (x,y)

readPositions :: String -> Either ParseError Pos
readPositions = parse parsePositions "stdin"

parseInput :: [String] -> [Pos]
parseInput  =  rights  . map readPositions

part1 :: FilePath -> IO [Pos]
part1  file= do i <- readInput file
                return $ parseInput i