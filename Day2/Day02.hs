import Data.Set (Set)
import Data.List
import qualified Data.Set as Set


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
