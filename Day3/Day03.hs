import Data.List
import Data.List.Split
import Data.Maybe
import Data.Either
import Text.Parsec.Char(spaces, digit, char)
import Text.Parsec.String(Parser)
import Text.Parsec(parse, many1,ParseError)
import qualified Data.Map.Strict as M

type Pos = (Int,Int)

data Claim = Claim {claimid::Int, pos::Pos, width::Int, height::Int}
    deriving (Show,Eq)

part1 :: FilePath -> IO Int
part1 file = do i <- readFile file 
                let allPoints = (getAllClaimPoints . rights . map readClaims . lines) i
                let mapOfClaims = foldr (\x ->  M.insertWith (+) x 1) M.empty allPoints
                let listOfClaims = M.toList mapOfClaims
                return $ length $ filter ( (>1) .snd) listOfClaims                



parseInt :: Parser Int
parseInt = read <$> many1 digit

getAllClaimPoints :: [Claim] -> [Pos]
getAllClaimPoints =  concatMap claimPoints 

pair :: Char -> Parser (Int,Int)
pair c = (,) <$> (spaces *> parseInt) <* char c <*> parseInt

claimPoints :: Claim -> [Pos]
claimPoints claim = [(x',y') | x' <- [x..x+w-1] , y' <- [y..y+h-1]]
                where (x,y) = pos claim
                      w = width claim
                      h = height claim

parseClaim :: Parser Claim
parseClaim = 
            char '#' >>
            parseInt >>= \id ->
            spaces >>
            char '@' >>
            pair ',' >>= \(x,y) ->
            char ':' >>
            pair 'x' >>= \(width,height) ->
            pure Claim {claimid = id,pos = (x,y) ,width = width,height = height}

           

readClaims :: String -> Either ParseError Claim
readClaims = parse parseClaim "stdin"
