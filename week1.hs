timesTen :: Int -> Int
timesTen x = 10 * x

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle r = pi * r^2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder height radius = area * height
    where area = areaOfCircle radius

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt( (y2-y1)^2 + (x2-x1)^2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0

isEven :: Int -> Bool
isEven x = divisibleBy x 2

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute x = if x > 0 then x else (x * (-1))