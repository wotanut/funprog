{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (concat, fst, head, reverse, snd, sum, tail, zip)
-- Definitions of the prelude functions fst and snd

fst (x, _) = x

snd (_, y) = y

-- Definitions of the prelude functions head and tail

head :: [p] -> p
head (x : _) = x
head [] = error "head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "tail: empty list"

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

-- For question 10
type StudentMark = (String, Int)

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

headPlusOne :: [Int] -> Int
headPlusOne [] = -1
headPlusOne (x :xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x : xs) = x : x : xs

rotate :: [a] -> [a]
rotate (x : y : xs) = y : x : xs
rotate xs = xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x :xs) = listLength xs + 1

multAll :: [Int] -> Int
multAll [] = 1
multAll (x : xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x : xs) = x && andAll xs

orAll :: [Bool] -> Bool
orAll [] = True
orAll (x : xs) = x || orAll xs

countIntegers :: Int -> [Int] -> Int
countIntegers _ [] = 0
countIntegers search (x : xs)
  | x == search = 1 + countIntegers search xs
  | otherwise = countIntegers search xs

removeAll :: Int -> [Int] -> [Int]
removeAll _ [] = []
removeAll search (x : xs)
    | x == search = removeAll search xs
    | otherwise = x : removeAll search xs

removeAllButFirst :: Int -> [Int] -> [Int]
removeAllButFirst _ [] = []
removeAllButFirst search (x :xs)
    | x == search = x: removeAll search xs
    | otherwise = x: removeAllButFirst search xs

listMarks :: String -> [StudentMark] -> [Int]
listMarks _ [] = []
listMarks student ((name, mark) : xs)
    | student == name = mark : listMarks student xs
    | otherwise = listMarks student xs

sorted :: [Int] -> Bool
sorted [_] = True
sorted (x : y :xs)
    | x <= y = sorted (y : xs)
    | otherwise = False

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix (x : xs) (y : ys)
    | x == y = prefix xs ys
    | otherwise = False

subSequence  :: [Int] -> [Int] -> Bool
subSequence  [] _ = True
subSequence  (x : xs) ( y : ys)
    | x == y = prefix xs ys
    | otherwise = subSequence xs ys