import Data.Set (Set)
import Data.List
import qualified Data.Set as Set

checkSum :: FilePath -> IO [String]
checkSum input = 
        do i <- readFile input
           let puzzles = lines i
           return puzzles
           
checkTwo:: String -> Int
checkTwo string | find [] charArray == 2 = 1
                | otherwise = 0
    where charArray =  (map (:[]) . sort) string
          find seen [] = 0
          find seen (x:xs) | x `elem` seen = 1 + find (x:seen) xs
                           | otherwise = find (x:seen) xs

                                      
checkThreeChar:: String -> Int
checkThreeChar string = find charArray
                 {-| find [] charArray == 3 = 1
                 | otherwise = 0-}
    where charArray = (map (:[]) . sort) string
          find [] = 0
          find (x:xs) | x == head xs  = 1 + find xs
                      | otherwise = find xs