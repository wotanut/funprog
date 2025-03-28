-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
  deriving (Eq, Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
  | m1 >= m2 = s1
  | otherwise = s2

-- Shapes algebraic type
data Shape = Circle Float | Rectangle Float Float
    deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
  deriving (Show)

data Building = Name String | Number Int
  deriving (Show)

-- Binary tree algebraic type
data Tree = Null | Node Int Tree Tree
  deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null)) (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving (Eq, Show, Read)

data Season = Winter | Spring | Summer | Autumn
  deriving (Eq, Show, Read)

season :: Month -> Season
season m
  | m `elem` [December, January, February] = Winter
  | m `elem` [March, April, May] = Spring
  | m `elem` [June, July, August] = Summer
  | otherwise = Autumn

numberOfDays :: Month -> Int -> Int
numberOfDays February year
  | year `mod` 4 == 0 = 29  -- Leap year
  | otherwise = 28
numberOfDays month _ -- Can't use divisible by 2 as strings
  | month `elem` [April, June, September, November] = 30
  | otherwise = 31

data Point = Cordinate Float Float
    deriving (Show)

data PositionedShape = PositionedShape Shape Point
    deriving (Show)

move :: PositionedShape -> Float -> Float -> PositionedShape
move (PositionedShape shape (Cordinate x y)) dx dy =
    PositionedShape shape (Cordinate (x + dx) (y + dy))

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node _ left right) = 1 + numberOfNodes left + numberOfNodes right

isMember :: Int -> Tree -> Bool
isMember _ Null = False
isMember x (Node n left right)
  | x == n = True
  | otherwise = isMember x right || isMember x left -- not a binary search tree

leaves :: Tree -> [Int]
leaves Null = []
leaves (Node n Null Null) = [n]
leaves (Node _ left right) = leaves left ++ leaves right

inOrder :: Tree -> [Int]
inOrder Null = []
inOrder (Node n left right) = inOrder left ++ [n] ++ inOrder right

testBinaryTree = Node 5 (Node 1 Null Null) (Node 8 (Node 7 Null Null) Null)

insert :: Int -> Tree -> Tree
insert x Null = Node x Null Null
insert x (Node n left right)
  | x < n = Node n (insert x left) right
  | x > n = Node n left (insert x right)
  | otherwise = Node n left right -- Do not insert duplicates, return the original tree

listToSearchTree :: [Int] -> Tree
listToSearchTree = foldl (flip insert) Null --I found foldl because I mistyped foldr

binaryTreeSort :: [Int] -> [Int]
binaryTreeSort = inOrder . listToSearchTree