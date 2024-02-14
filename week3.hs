{-
-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||))

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a

fact :: Int -> Int
fact n
  | n == 0 = 1
  | n > 0 = n * fact (n - 1)
  | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m
  | n == 0 = 0
  | n > 0 = m + mult (n - 1) m
  | otherwise = - mult (- n) m

divide :: Int -> Int -> Int
divide n m
  | n < m = 0
  | otherwise = 1 + divide (n - m) m
-}

import Prelude hiding (gcd, (&&), (||))
infixr 3 &&


-- examples

nor :: Bool -> Bool -> Bool
nor False x = not x
nor True x = False

fibonacci :: Int -> Int
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 1
(&&) :: Bool -> Bool -> Bool
False && _ = False
True && x = x

-- False && False = False
-- False && True = False
-- True && False = False
-- True && True = True

-- 2

-- exOr :: Bool -> Bool -> Bool
-- exOr False True = True
-- exOr True False = True
-- exOr a b = False

-- exOr :: Bool -> Bool -> Bool
-- exOr True b = not b
-- exOr a True = not a
-- exOr _ _ = False

exOr :: Bool -> Bool -> Bool
exOr a b = a /= b


-- 3
ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x _ = x
ifThenElse _ _ y = y

--4
daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

validDate :: Int -> Int -> Bool
validDate d m = (d > 0) && (d <= daysInMonth m)

--5
sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

--6
sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n ^ 2 + sumSquares (n - 1)

--7
power :: Int -> Int -> Int
power _ 0 = 1
power x y = x * power x (y - 1)

--8 
sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | x > y = 0
  | otherwise = x + sumFromTo (x + 1) y

--9
absolute :: Int -> Int
absolute number
  | number >= 0 = number
  | otherwise = -number

gcd :: Int -> Int -> Int
gcd x y
  | x == y = x
  | otherwise = gcd (abs (x - y)) (min x y)
  -- | x == y = x
  -- | x > y = gcd difference y
  -- | otherwise = gcd x difference
  -- where
  --   difference = absolute (x - y)

--10
intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n x
  | x ^ 2 > n = findRoot n (x - 1)
  | otherwise = x
