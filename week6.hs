{- Week6.hs
 This module illustrates the use of functions as values
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.Char

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Float] -> [Float]
keepPositive = filter (>=0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- map: Apply a function to all elements
-- filter: Keep only elements that pass a check
-- foldr: Fold a list down to a single value
mult10 :: [Int] -> [Int]
mult10 = map (*10)

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

orAll f = foldr (||) False f

sumSquares xs = sum (map (^2) xs)

-- without
zeroToTen :: [Int] -> [Int]
zeroToTen [] = []
zeroToTen (x:xs)
    | x >= 0 && x <= 10 = x : zeroToTen xs
    | otherwise = zeroToTen xs

-- lambda
-- zeroToTen :: [Int] -> [Int]
-- zeroToTen = filter (\x -> x >= 0 && x <= 10)

squareRoots :: [Float] -> [Float]
squareRoots xs = map sqrt (keepPositive xs)

countBetween :: Float -> Float -> [Float] -> Int
countBetween lower upper xs = length (filter (\x -> x >= lower && x <= upper) xs)

-- Write three definitions of this function similar to the first worked example. One of which should be a lambda
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f = foldr (\x acc -> f x > 0 && acc) True

-- alwaysPositive :: (Float -> Float) -> [Float] -> Bool
-- alwaysPositive f xs = length (filter (>= 0) (map f xs)) == length xs

-- alwaysPositive f list = all ((>= 0) . f) list

productSquareRoots :: [Float] -> Float
productSquareRoots list = foldr (*) 1 (squareRoots list)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst _ [] = []
removeFirst p (x:xs)
    | p x = xs
    | otherwise = x : removeFirst p xs

removeLast :: (a -> Bool) -> [a] -> [a]
removeLast p = Prelude.reverse . removeFirst p . Prelude.reverse

reverse :: [a] -> [a]
reverse a = foldr (\x xs -> xs ++ [x]) [] a