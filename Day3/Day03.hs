-- import Data.Set
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
--import qualified Data.Set as Set
import Text.Parsec.Char(spaces, digit, char)
import Text.Parsec.String(Parser)
import Text.Parsec(parse, many1,ParseError)
import qualified Data.Map as M


data Claim = Claim {claimid, x , y, width, height:: Int}
    deriving (Show,Eq)

part1 :: FilePath -> IO [Claim]
part1 file = do i <- readFile file
                let claims = (rights . map readClaims . lines) i
                return claims

parseInt :: Parser Int
parseInt = read <$> many1 digit

pair :: Char -> Parser (Int,Int)
pair c = (,) <$> (spaces *> parseInt) <* char c <*> parseInt

parseClaim :: Parser Claim
parseClaim = do
            char '#'
            id <- parseInt
            spaces
            char '@'
            (x,y) <- pair ','
            char ':'
            (width,height) <- pair 'x'
            pure $ Claim {claimid = id,x = x ,y = y ,width = width,height = height}

readClaims :: String -> Either ParseError Claim
readClaims = parse parseClaim "stdin"
