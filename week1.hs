-- 1
timesTen :: Int -> Int
timesTen n = n * 10

-- 2
sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

-- 3
areaOfCircle :: Float -> Float
areaOfCircle area = pi * (area ^ 2)

-- 4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder h r = pi * r^2 * h

-- 5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt (((y1 - y2)^2) + ((x1 - x2)^2))

-- 6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c = a /= b && c /= b && a /= c

-- 7
divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0

-- 8
isEven :: Int -> Bool
isEven = even