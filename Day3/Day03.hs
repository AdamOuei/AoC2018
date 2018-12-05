import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
import Text.Parsec.Char(spaces, digit, char)
import Text.Parsec.String(Parser)
import Text.Parsec(parse, many1,ParseError)
import qualified Data.Map.Strict as M


data Claim = Claim {claimid, x , y, width, height:: Int}
    deriving (Show,Eq)

part1 :: FilePath -> IO [Claim]
part1 file = readFile file >>= \i -> 
             return $ (rights . map readClaims . lines) i
                

parseInt :: Parser Int
parseInt = read <$> many1 digit

pair :: Char -> Parser (Int,Int)
pair c = (,) <$> (spaces *> parseInt) <* char c <*> parseInt

parseClaim :: Parser Claim
parseClaim = 
            char '#' >>
            parseInt >>= \id ->
            spaces >>
            char '@' >>
            pair ',' >>= \(x,y) ->
            char ':' >>
            pair 'x' >>= \(width,height) ->
            pure Claim {claimid = id,x = x ,y = y ,width = width,height = height}

           

readClaims :: String -> Either ParseError Claim
readClaims = parse parseClaim "stdin"
