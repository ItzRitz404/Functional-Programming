import Data.Char

testData :: [StudentMark]
testData =
  [ ("John", 53),
    ("Sam", 16),
    ("Kate", 85),
    ("Jill", 65),
    ("Bill", 37),
    ("Amy", 22),
    ("Jack", 41),
    ("Sue", 71)
  ]

sumEvenNumbersBetween :: Int -> Int -> Int
sumEvenNumbersBetween x y = sum [i | i <- [x .. y], mod i 2 == 0]
sumEvenNumbersBetween x y
    | x > y = 0
    | mod x 2 == 0 = x + sumEvenNumbersBetween (x + 2) y
    | otherwise = sumEvenNumbersBetween (x + 1) y

type StudentMark = (String, Int)

averageMark :: [StudentMark] -> Float
averageMark [] = 0
averageMark stmks = fromIntegral sumMarks / fromIntegral numberOfStudents
    where
        sumMarks = sum [mk | (_ , mk) <- stmks]
        numberOfStudents = length stmks

-- 1
sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x + y, x - y)

--2
grade :: StudentMark -> Char
grade(_, mark)
    | mark > 100 || mark < 0 = error "Invalid mark"
    | mark >= 70 = 'A'
    | mark >= 60 = 'B'
    | mark >= 50 = 'C'
    | mark >= 40 = 'D'
    | otherwise = 'F'

--3
capMark :: StudentMark -> StudentMark
capMark (name, mark)
    | mark > 100 || mark < 0 = error "Invalid mark"
    | mark >= 40 = (name, 40)
    | otherwise = (name, mark)

--4
firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]

--5
firstSquares :: Int -> [Int]
firstSquares n = [x^2 | x <- [1..n]]

--6
capitalise :: String -> String
capitalise n = [toUpper x | x <- n]

--7
onlyDigits :: String -> String
onlyDigits n = [x| x <- n, isDigit x]

--8
capMarks :: [StudentMark] -> [StudentMark]
capMarks stmks = [capMark (name, mark) | (name, mark) <- stmks]

--9
gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents stmks = [(name, grade (name, mark)) | (name, mark) <- stmks]

--10
