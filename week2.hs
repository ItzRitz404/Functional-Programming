import Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)

-- example 1
heartMonitor :: Int -> Int -> String
heartMonitor age bpm
  | age > 80 && bpm > 100 = "High heart rate for +80!"
  | age > 60 && age <= 80 && bpm > 130 = "High heart rate for 60-80!"
  | age > 40 && age <= 60 && bpm > 140 = "High heart rate for 40-60!"
  | age > 20 && age <= 40 && bpm > 155 = "High heart rate for 20-40!"
  | age >= 0 && age <= 20 && bpm > 170 = "High heart rate for 0-20!"
  | otherwise = "Normal heart rate"

-- example 2
pizzaCalories :: Int -> String -> Float
pizzaCalories diameter toppings = (11.5 + toppingCalories) * area
  where
    area = pi * (fromIntegral diameter / 2) ^ 2
    toppingCalories
      | toppings == "pepperoni" = 6
      | toppings == "tuna" = 4
      | toppings == "veggie" = 2.5
      | otherwise = 0

-- 1
absolute :: Int -> Int
absolute number
  | number >= 0 = number
  | otherwise = -number

-- 2
sign :: Int -> Int
sign number
  | number > 0 = 1
  | number == 0 = 0
  | otherwise = -1

-- 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
  | a == b && b == c = 3
  | a == b || a == c || b == c = 2
  | otherwise = 0

-- 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths s1 s2 s3 = diagonalLength (s1 + s2 + s3)
  where
    diagonalLength sideLength = sqrt (2 * sideLength ^ 2)

-- 5
taxiFare :: Int -> Float
taxiFare d = 2.20 + rest
  where
    rest
      | d <= 10 = 0.5 * fromIntegral d
      | otherwise = 5 *  fromIntegral (d - 10) * 0.30
-- taxiFare km
--   | km <= 10 = 2.2 + 0.5 * fromIntegral km
--   | otherwise = 7.2 + 0.3 * fromIntegral km

-- 6
-- howManyAboveAverage :: Int -> Int -> Int -> Int
-- howManyAboveAverage a b c
--   -- | a > average && b > average && c > average = 3
--   | f a > average && f b > average || a > average && c > average || b > average && c > average = 2
--   | a > average || b > average || c > average = 1
--   | otherwise = 0
--   where
--     average = fromIntegral (a + b + c) / 3
--     f = fromIntegral

-- 7
validDate :: Int -> Int -> Bool
validDate day month
  | month < 1 || month > 12 = False
  | month `elem` [1, 3, 5, 7, 8, 10, 12] && day <= 31 = True
  | month `elem` [4, 6, 9, 11] && day <= 30 = True
  | month == 2 && day <= 28 = True
  | otherwise = False

-- 8
daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  | month `elem` [4, 6, 9, 11] = 30
  | month == 2 && isLeapYear year == 0 = 29
  | otherwise = 28
  where
    isLeapYear year = year `mod` 4

-- written exercises

{-

sumThree 3 5 7
  3 + 5 + 7      Def of sumThree
  8 + 7          Arithmetic
  15             Arithmetic

sumThree 8 (1 + 3) 2
  8 + (1 + 3) + 2     Def of sumThree
  8 + 4 + 2           Arithmetic
  12 + 2              Arithmetic
  14                  Arithmetic

threeDifferent 1 4 2
  1 /= 4 && 4 /= 2 && 1 /= 2     Def of threeDifferent
  True && 4 /= 2 && 1 /= 2       Comparison
  True && True && 1 /= 2         Comparison
  True && True && True           Comparison
  True && True                   Comparison
  True                           Result

threeDifferent 1 7 7
  1 /= 7 && 7 /= 7 && 1 /= 7     Def of threeDifferent
  True && False && 1 /= 7        Comparison
  True && False && True          Comparison
  True && False                  Comparison
  False                          Comparison

howManyEqual 3 5 2
  3 == 5 || 3 == 2 || 5 == 2     Def of howManyEqual
  False || 3 == 2 || 5 == 2      Comparison
  False || False || 5 == 2       Comparison
  False || False || False        Comparison
  False || False                 Comparison
  False                          Comparison
  0                              Result

howManyEqual 5 2 5
  5 == 2 || 5 == 5 || 2 == 5     Def of howManyEqual
  False || 5 == 5 || 2 == 5      Comparison
  False || True || 2 == 5        Comparison
  False || True || False         Comparison
  False || True                  Comparison
  True                           Comparison
  1                              Result
-}