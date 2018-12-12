import Data.List
import Data.Ord(comparing)
import Data.Char
import Data.Maybe



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
buildList :: String -> [String]
buildList string = map (\x -> removeOneLetter x string) ['a'..'z']


removeOneLetter :: Char -> String-> String
removeOneLetter c = filter (\x -> toLower x /= c  )

doPart2 :: [String] -> Int
doPart2 = length . minimumBy (comparing length) . map removeAllUnits . buildList . unwords

part1 :: FilePath -> IO Int
part1 file = do i <- readInput file
                return $ (length . removeAllUnits . unwords) i

part2 :: FilePath -> IO Int
part2 file = do i <- readInput file
                return $ doPart2 i

