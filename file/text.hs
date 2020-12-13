import System.Environment (getArgs)
import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&))

//Counting the frequency of occurrence of words in a file

main = do
    args <- getArgs
    let inputFile = head args
    fileContent <- readFile inputFile
    let mapOfWords = (wordCount fileContent)
    print  mapOfWords
    let size =  lengthX mapOfWords
    print size
    let floatSize = fromIntegral size :: Float
    let result = map (devideX floatSize) mapOfWords
    print result

wordCount :: String -> [(String, Int)]
wordCount = map (head &&& length) . group . sort . words . map toLower    

lengthX :: [(String, Int)] -> Int
lengthX [] = 0
lengthX ((a, b):xs) = b + lengthX xs

devideX :: Float -> (String, Int) -> (String, Float)
devideX x (a,x2) = (a, (fromIntegral x2 :: Float) /  x)
