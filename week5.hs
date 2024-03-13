{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}
-- import Main (duplicate)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)

type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65), ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head (x : _) = x

tail (_ : xs) = xs

absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x : xs) = abs x

sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x : xs) = 2 * x : doubleAll xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a, b)]
zip (x : xs) (y : ys) = (x, y) : zip xs ys
zip _ _ = []

-- 1
headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x : _) = x + 1

-- 2
duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x : x : xs

-- 3
rotate :: [a] -> [a]
-- rotate [] = []
-- rotate [x] = [x]
rotate (x : y : xs) = y : x : xs
rotate i = i

-- 4
listLength :: [a] -> Int
listLength [] = 0
listLength (_ : xs) = 1 + listLength xs

-- 5
multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * multAll xs

-- 6
andAll :: [Bool] -> Bool
andAll [] = True
andAll (x : xs) = x && andAll xs

-- 7
orAll :: [Bool] -> Bool
orAll [] = False
orAll (x : xs) = x || orAll xs

-- 8
countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers n (x : xs)
  | n == x = 1 + countIntegers n xs
  | otherwise = countIntegers n xs

-- 9
removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll n (x : xs)
  | n == x = removeAll n xs
  | otherwise = x : removeAll n xs

-- 10
removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst n (x : xs)
  | n == x = x : removeAll n xs
  | otherwise = x : removeAllButFirst n xs

-- removeAllButFirst 3 [1,2,3,5,3,4] = [1,2,3,5,4]

-- 11
listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks name ((n, m) : xs)
  | name == n = m : listMarks name xs
  | otherwise = listMarks name xs

-- 12
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x : y : xs)
  | x <= y = sorted (y : xs)
  | otherwise = False

-- 13
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x : xs) (y : ys)
  | x == y = prefix xs ys
  | otherwise = False

-- 14
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence xs (y : ys)
  | prefix xs (y : ys) = True
  | otherwise = subSequence xs ys