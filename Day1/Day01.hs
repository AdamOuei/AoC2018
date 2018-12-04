

-- Day One
sumFile :: FilePath -> IO Int
sumFile files = do
    file <- readFile files
    let array = map ((read::String->Int) . fix) $ lines file
    return (sum array)
    
     
    

fix :: String -> String
fix s | head s == '+' = tail s
      | otherwise = s

-- Part 2
frequencyTwice ::  FilePath ->IO Int
frequencyTwice files= do 
    file <- readFile files
    return $ findDuplicate . scanl (+) 0 . cycle . map ((read::String-> Int) . fix ) $ lines file
    


findDuplicate :: [Int] -> Int
findDuplicate = find []
            where find seen [] = 0 
                  find seen (x:xs) | x `elem` seen = x
                              | otherwise = find (x:seen) xs