import Data.Set (Set)
import Data.List
import Data.Maybe
import qualified Data.Set as Set

-- Part 1
alphabetString  = map (:[]) "abcdefghijklmnopqrstuvwxyz"

checkSum :: FilePath -> IO Int
checkSum input = 
        do i <- readFile input
           let puzzles = lines i
           let sumWithTwo = sum $ map checkTwo puzzles
           let sumWithThree = sum $ map checkThree puzzles
           return (sumWithTwo * sumWithThree)
           
checkTwo:: String -> Int
checkTwo string |   2 `elem` map (isElem charArray) alphabetString = 1
                | otherwise = 0
    where charArray =  (map (:[]) . sort) string
      
checkThree :: String -> Int
checkThree string |   3 `elem` map (isElem charArray) alphabetString = 1
                | otherwise = 0
    where charArray =  (map (:[]) . sort) string


isElem :: [String] -> String -> Int
isElem [] element = 0
isElem (x:xs) element |  x == element = 1 + isElem xs element
                      | otherwise = isElem xs element

-- Part 2
findFabric :: FilePath -> IO String
findFabric file = do i <- readFile file
                     let id = lines i
                     let (s1,s2) =  findPair id
                     return $ sameCharacters s1 s2
 
pairOfStrings :: String -> [String] -> Maybe (String,String)
pairOfStrings theString [] = Nothing
pairOfStrings theString listOfStrings@(x:xs) | oneOff theString x = Just (theString,x)
                                             | otherwise = pairOfStrings theString xs


findPair :: [String] -> (String,String)
findPair (x:xs) = fromMaybe (findPair xs) (pairOfStrings x xs)

oneOff :: String-> String -> Bool
oneOff string1 string2 = help string1 string2 0
      where help [] [] 0 = True
            help [] [] 1 = True
            help _ _ 2 = False
            help (x:xs) (y:ys) 0 | x /= y = help xs ys 1 
                                 | otherwise = help xs ys 0
            help (x:xs) (y:ys) 1 | x/=y = False
                                 | otherwise = help xs ys 1


sameCharacters :: String -> String -> String
sameCharacters [] [] = []
sameCharacters (x:xs) (y:ys) | x == y = x : sameCharacters xs ys
                             | otherwise = sameCharacters xs ys