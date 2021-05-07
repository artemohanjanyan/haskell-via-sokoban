module Main where

import Data.Char (isControl, isAscii, isDigit)
import Data.Function (on)
import Data.List (inits, tails, maximumBy, intercalate, groupBy, group, sort)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Text.Read (readMaybe)

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

safeString :: String -> String
safeString = map replace
  where
    replace c
      | isControl c || not (isAscii c) = '_'
      | otherwise = c

holes :: [a] -> [[a]]
holes xs = zipWith (++) (inits xs) (tail (tails xs))

longestText :: Show a => [a] -> a
longestText = maximumBy (comparing (length . show))

adjacents :: [a] -> [(a, a)]
adjacents [] = []
adjacents as@(_:bs) = zip as bs

commas :: [String] -> String
commas = intercalate ", "

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldr (zipWith (+)) (repeat 0)

sumNumbers :: String -> Integer
sumNumbers = sum . mapMaybe readMaybe . groupBy ((==) `on` isDigit)

wordCount :: String -> String
wordCount str =
  "Number of lines: " ++ show lineN ++ "\n" ++
  "Number of empty lines: " ++ show emptyLineN ++ "\n" ++
  "Number of words: " ++ show wordN ++ "\n" ++
  "Number of unique words: " ++ show uniqueWordN ++ "\n" ++
  "Number of words followed by themselves: " ++ show repeatedWordN ++ "\n" ++
  "Length of the longest line: " ++ show longestLineLength ++ "\n"
  where
    strLines = lines str
    strWords = words str

    lineN = length strLines
    emptyLineN = length $ filter null strLines
    wordN = length strWords
    uniqueWordN = length $ group $ sort strWords
    repeatedWordN = length $ filter (uncurry (==)) $ adjacents strWords
    longestLineLength = length $ longestText strLines

ex_halveEvens =
  [ halveEvens [] == []
  , halveEvens [1,2,3,4,5] == [1,2]
  , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
  ]

ex_safeString =
  [ safeString [] == []
  , safeString "Hello World!" == "Hello World!"
  , safeString "That’s your line:\n" == "That_s your line:_"
  , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
  ]

ex_holes =
  [ holes "" == []
  , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
  ]

ex_longestText =
  [ longestText [True,False] == False
  , longestText [2,4,16,32] == (32::Int)
  , longestText (words "Hello World") == "World"
  , longestText (words "Olá mundo") ==  "Olá"
  ]

ex_adjacents =
  [ adjacents "" == []
  , adjacents [True] == []
  , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
  ]

ex_commas =
  [ commas [] == ""
  , commas ["Hello"] == "Hello"
  , commas ["Hello", "World"] == "Hello, World"
  , commas ["Hello", "", "World"] == "Hello, , World"
  , commas ["Hello", "new", "World"] == "Hello, new, World"
  ]

ex_addPolynomials =
  [ addPolynomials [[]] == []
  , addPolynomials [[0, 1], [1, 1]] == [1, 2]
  , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
  ]

ex_sumNumbers =
  [ sumNumbers "" == 0
  , sumNumbers "Hello world!" == 0
  , sumNumbers "a1bc222d3f44" == 270
  , sumNumbers "words0are1234separated12by3integers45678" == 46927
  , sumNumbers "000a." == 0
  , sumNumbers "0.00a." == 0
  ]

testResults :: [(String, [Bool])]
testResults =
  [ ("halveEvens",      ex_halveEvens)
  , ("safeString",      ex_safeString)
  , ("holes",           ex_holes)
  , ("longestTest",     ex_longestText)
  , ("adjacents",       ex_adjacents)
  , ("commas",          ex_commas)
  , ("addPolynomials",  ex_addPolynomials)
  , ("sumNumbers",      ex_sumNumbers)
  ]

formatTests :: [(String, [Bool])] -> String
formatTests = unlines . map formatTest
  where
    formatTest (func, results) =
      func ++ ": " ++ successN ++ "/" ++ allN ++ " successful tests." ++
      (if failingN > 0 then failingDetails else "")
      where
        successN = show $ length $ filter id results
        failingN = length $ filter (not . id) results
        allN = show $ length results
        failingDetails = " Failing tests: " ++ formatIndices results 
        formatIndices = withCommas . map fst . filter (not . snd) . zip [1..]
        withCommas [] = ""
        withCommas [x] = show x
        withCommas xs = intercalate ", " (map show $ init xs) ++ " and " ++ show (last xs)

main :: IO ()
main = putStr $ formatTests testResults
