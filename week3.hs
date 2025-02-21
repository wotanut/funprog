-- We don't import '||' from the prelude, so that we can
-- define our own version

import Prelude hiding ((||),(&&), gcd)

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >).

infixr 2 ||
infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

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

exOr :: Bool -> Bool -> Bool
exOr False True = True
exOr True False = False
exOr _ _ = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True result1 _ = result1
ifThenElse False _ result2 = result2

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31
-- Once you have written and tested your daysInMonth function, write a new (simpler)
-- version of validDate from the previous worksheet (guards or patterns should not be
-- needed

-- Old function for reference
-- validDate :: Int -> Int -> Bool
-- validDate day month
--     | month > 12 || month < 0 || day < 0 || day > 31 = False
--     | month == 2 && day == 28 = True
--     | month `mod` 2 == 0 && day <= 30 = True
--     | month `mod` 2 == 1 && day <= 31 = True
--     | otherwise = False

validDate :: Int -> Int -> Bool
validDate day month = 
    day > 0 && day <= daysInMonth month && month > 0 && month <= 12




-- sumNumbers :: Int -> Int
-- sumNumbers n
--     | n == 0 = 0
--     | otherwise = n + sumNumbers (n - 1)

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers n = n + sumNumbers (n - 1)

-- sumSquares :: Int -> Int
-- sumSquares n
--     | n == 0 = 0
--     | n > 0 = n^2 + sumSquares (n - 1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares n = n^2 + sumSquares (n - 1)

-- power :: Int -> Int -> Int
-- power number square 
--     | square == 0 = 1
--     | otherwise = number * power number (square - 1)

power :: Int -> Int -> Int
power _ 0 = 1
power number square = number * power number (square - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo first second
    | second < first = 0
    | second == first = first
    | otherwise = second + sumFromTo first (second - 1)

gcd :: Int -> Int -> Int
gcd a b
    | a == b = a -- If they are equal, return either
    | a > b = gcd (a - b) b -- If a is bigger than b call itself with the difference being the first argument
    | otherwise = gcd a (b - a) -- A is smaller so call itself with the difference being the second argument
    -- essentially you're going through making the bigger number smaller than the other number until they're equal

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot a b
    | b * b <= a = b -- stop when b * b is less than a
    | otherwise = findRoot a (b -1)