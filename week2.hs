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
sumDiagonalLengths s1 s2 s3 = diagonalLength s1 + diagonalLength s2 + diagonalLength s3
  where
    diagonalLength sideLength = sqrt (2 * sideLength ^ 2)

--5
taxiFare :: Int -> Float
