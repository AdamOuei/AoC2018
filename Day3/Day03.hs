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
                let mapOfClaims = foldr (\x -> M.insertWith (+) x 1) M.empty allPoints
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



-- Part 2
getAllClaimTriplets :: [Claim] -> [(Pos,Int)]
getAllClaimTriplets =  concatMap claimTriplets


claimTriplets :: Claim -> [(Pos,Int)]
claimTriplets claim = [((x',y'),cid) | x' <- [x..x+w-1] , y' <- [y..y+h-1]]
                where (x,y) = pos claim
                      w = width claim
                      h = height claim
                      cid = claimid claim
getAllClaimIds :: [Claim]-> [Int]
getAllClaimIds =  map claimIds

claimIds :: Claim -> Int
claimIds = claimid 
                
                      
part2 :: FilePath -> IO [Int]
part2 file = do i <- readFile file 

                let --input =  (rights . map readClaims . lines) i
                    allClaims = (getAllClaimTriplets . rights . map readClaims . lines) i
                    allClaimIds = (getAllClaimIds . rights . map readClaims . lines) i
                    mapOfClaims = foldr (\ (x, y) -> M.insertWith (++) x [y] ) M.empty allClaims
                    listOfClaims = M.toList mapOfClaims
                    listOfMapIds = filter (\x -> length x >1) (map snd listOfClaims)
                    anotherList = (sort .nub . concat) listOfMapIds
                    lastList = filter (`notElem` anotherList) allClaimIds
                    in
                        return lastList