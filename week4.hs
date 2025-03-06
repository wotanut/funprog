import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1, m1) (s2, m2)
  | m1 >= m2 = s1
  | otherwise = s2

marks :: [StudentMark] -> [Int]
marks stmks = [mk | (st, mk) <- stmks]

pass :: [StudentMark] -> [String]
pass stmks = [st | (st, mk) <- stmks, mk >= 40]

-- An example list of student marks
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

addPairs :: [(Int, Int)] -> [Int]
addPairs pairList = [i + j | (i, j) <- pairList]

minAndMax :: Int -> Int -> (Int, Int)
minAndMax x y
  | x <= y = (x, y)
  | otherwise = (y, x)

sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x + y, x - y)

grade :: StudentMark -> Char
grade (_, mark)
  | mark > 100 || mark < 0 = error "Grade not between 100 and 0"
  | mark >= 70 = 'A'
  | mark >= 60 = 'B'
  | mark >= 50 = 'C'
  | mark >= 40 = 'D'
  | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (student, mark)
  | mark > 100 || mark < 0 = error "Grade not between 100 and 0"
  | mark >= 40 = (student, 40)
  | otherwise = (student,mark)

firstNumbers :: Int -> [Int]
firstNumbers n
  | n <= 0 = []
  | otherwise = [x | x <- firstNumbers (n - 1)] ++ [n]

firstSquares :: Int -> [Int]
firstSquares n
  | n <= 0 = []
  | otherwise = [x^2 | x <- firstNumbers (n - 1)] ++ [n^2]

capitalise :: String -> String
capitalise word = [toUpper c | c <- word] 

onlyDigits :: String -> String
onlyDigits word = [c | c <- word, isDigit c]

capMarks :: [StudentMark] -> [StudentMark]
capMarks marks = [capMark stmrks | stmrks <- marks]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents marks = [(student, grade (student, stmrks)) | (student, stmrks) <- marks]

-- Guards
duplicate:: String -> Int -> String
duplicate quote n
  | n == 0 = ""
  | otherwise = concat [quote, duplicate (quote) (n - 1)]

-- List Comprehension
-- duplicate:: String -> Int -> String
-- duplicate quote n = concat [quote | _ <- [1..n]]

divisors :: Int -> [Int]
divisors x = [i | i <- [1 .. x], mod x i == 0]

isPrime :: Int -> Bool
isPrime x
    | divisors x == [1] = True -- For 1
    | divisors x == [1,x] = True
    | otherwise = False

-- fst and snd
split :: [(a, b)] -> ([a], [b])
split pairs = ([fst first | first <- pairs], [snd second | second <- pairs])

-- Tuple unpacking
-- split :: [(a, b)] -> ([a], [b])
-- split pairs =( [first | (first, _) <- pairs],[second | (_, second) <- pairs])