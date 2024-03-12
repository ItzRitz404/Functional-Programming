{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

--1
mult10 :: [Int] -> [Int]
mult10 = map (*10)

--2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower


--3
orAll :: [Bool] -> Bool
orAll = foldr (||) False

--4
sumSquare :: [Int] -> Int
sumSquare = foldr (\x y -> x^2 + y) 0

--5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> x >= 0 && x <= 10)

--6
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt

--7
countBetween :: Float -> Float -> [Float] -> Int
countBetween x y = length . filter (\n -> n >= x && n <= y)

--8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive n = and . map (\x -> n x > 0)

--9
productSquareRoots :: [Float] -> Float
productSquareRoots = foldr (\x n -> if x >= 0 then sqrt x * n else n) 1.0