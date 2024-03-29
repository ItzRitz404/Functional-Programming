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
doubleAll = map (* 2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (> 0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- 1
mult10 :: [Int] -> [Int]
mult10 = map (* 10)

-- 2
onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

-- 3
orAll :: [Bool] -> Bool
-- orAll = foldr (||) False
orAll [] = False
orAll (x:xs) = x || orAll xs

-- 4
sumSquare :: [Int] -> Int
-- sumSquare = foldr ((+) . (^ 2)) 0
sumSquare= foldr (+) 0 . map (^2) 
-- map thn foldr
-- sumSquare = foldr (\x y -> x^2 + y) 0

-- 5
zeroToTen :: [Int] -> [Int]
zeroToTen = filter (<10) . filter (>0)

-- 6
squareRoots :: [Float] -> [Float]
squareRoots = map sqrt 

-- 7
countBetween :: Float -> Float -> [Float] -> Int
countBetween x y = length . filter (\n -> n >= x && n <= y)

-- 8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive n = and . map (\x -> n x > 0)
alwaysPositive  f xs = length (filter (>=0) (map f xs)) == length xs

-- 9
productSquareRoots :: [Float] -> Float
productSquareRoots = foldr squareRoot 1.0
  where
    squareRoot x n
      | x >= 0 = sqrt x * n
      | otherwise = n

-- 10
removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst f (x : xs)
  | f x = xs
  | otherwise = x : removeFirst f xs

-- 11
removeLast :: (a -> Bool) -> [a] -> [a]
-- removeLast f x = reverse (removeFirst f (reverse x))
removeLast x = reverse . removeFirst x . reverse

-- 12
zeroToTen2 :: [Int] -> [Int]
zeroToTen2 = filter (\x -> x >= 0 && x <= 10)

-- 13
-- a
-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive n = and . map (\x -> n x > 0)

-- b
productSquareRoots1 :: [Float] -> Float
productSquareRoots1 = foldr (\x n -> if x >= 0 then sqrt x * n else n) 1.0

-- c
removeLast1 :: (a -> Bool) -> [a] -> [a]
removeLast1 n xs = reverse (removeFirst n (reverse xs))