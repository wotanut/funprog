absolute :: Int -> Int
absolute number
    | number < 0 = (- number)
    | number >= 0 = number

sign :: Int -> Int
sign number
    | number == 0 = 0
    | otherwise = absolute number

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z = 3
    | x == y || y == z || x == z = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = (diagnoal x) + (diagnoal y) + (diagnoal z)
    where diagnoal number = sqrt (2 * (number)^2)

taxiFare :: Int -> Float
taxiFare distance
    | distance < 1 = 2.2
    | distance < 10 = 2.2 + fromIntegral (distance * 50) / 100
    | otherwise = 2.2 + fromIntegral (10 * 50) / 100 + fromIntegral ((distance - 10) * 30) /100

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    | fromIntegral x < average && fromIntegral y < average && fromIntegral z < average = 3
    | (fromIntegral x < average && fromIntegral y < average) || (fromIntegral y < average && fromIntegral z < average) || (fromIntegral x < average && fromIntegral z <average) = 1
    | otherwise = 3
    where average = fromIntegral (x + y + z) / 3.0

validDate :: Int -> Int -> Bool
validDate day month
    | month > 12 || month < 0 || day < 0 || day > 31 = False
    | month == 2 && day == 28 = True
    | month `mod` 2 == 0 && day <= 30 = True
    | month `mod` 2 == 1 && day <= 31 = True
    | otherwise = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month == 2 && year `mod` 4 == 0 = 29
    | month == 2 && year `mod` 4 /= 0 = 28
    | validDate 31 month = 31
    | otherwise = 30

-- Question 1
------------------------------------------------------------------------------------------------------------------------
-- For your sumThree function from worksheet 1, write down the calculations that evaluate the following expressions:
-- • sumThree 3 5 7
-- 3 + 5 + 7 =15

-- • sumThree 8 (1 + 3) 2
-- 8 + 4 + 2 = 14


-- Question 2
------------------------------------------------------------------------------------------------------------------------
-- For your threeDifferent function from worksheet 1, write the calculations that evaluate the following expressions:
-- • threeDifferent 1 4 2
-- 1 != 4 & 4 != 2 & 2 != 1 = true

-- • threeDifferent 1 7 7
-- ! != 7 & 7 != 7 = false


-- Question 3
------------------------------------------------------------------------------------------------------------------------
-- For your howManyEqual function from this worksheet, write the calculations that evaluate the following expressions:
-- • howManyEqual 3 5 2
-- 3 != 5 & 5 != 2 && 2 != 3 = 0

-- • howManyEqual 5 2 5
-- 5 != 2 && 2 != 5 && 5 == 5 = 2