import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Prelude hiding (Left, Right)

-- 1 ----------------------------------------------------------------------
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- 3 ----------------------------------------------------------------------
mean :: (Fractional a) => [a] -> a
mean xs = s / l
    where s = sum xs
          l = fromIntegral $ length xs

-- 4 ----------------------------------------------------------------------
palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

palindrome' :: [a] -> [a]
palindrome' xs = xs ++ myReverse xs
    where myReverse [] = []
          myReverse (y:ys) = myReverse ys ++ [y]

palindrome'' :: [a] -> [a]
palindrome'' xs = xs ++ myReverse xs
    where myReverse = foldl (flip $ (:)) []

-- 5 ----------------------------------------------------------------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome [_] = True
isPalindrome (x:xs)
    | x == last xs = isPalindrome $ init xs
    | otherwise = False

-- 6 ----------------------------------------------------------------------
sortByLength :: (Ord a) => [[a]] -> [[a]]
sortByLength xxs = sortBy (compare `on` length) xxs

sortByLength' :: (Ord a) => [[a]] -> [[a]]
sortByLength' = sortBy (comparing length)

-- 7 ----------------------------------------------------------------------
intersperse' :: a -> [[a]] -> [a]
intersperse' _ [] = []
intersperse' _ (x:[]) = x
intersperse' n (x:xs) = x ++ [n] ++ intersperse' n xs

intersperse'' :: a -> [[a]] -> [a]
intersperse'' _ [] = []
intersperse'' n (f:xs) = f ++ foldl (\acc v -> acc ++ (n:v)) [] xs

intersperse''' :: a -> [[a]] -> [a]
intersperse''' _ [] = []
intersperse''' n xs = foldl1 ((++) . (++ [n])) xs

-- 8 ----------------------------------------------------------------------
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height :: Tree a -> Int
height Empty = 0
height (Node _ Empty Empty) = 1
height (Node _ l r) = 1 + max hl hr
    where hl = height l
          hr = height r

-- 9 ----------------------------------------------------------------------
data Direction = Left | Right | Straight deriving (Show, Eq)
data Point2D = Point2D { x :: Double, y :: Double } deriving Show

-- 10 ---------------------------------------------------------------------
direction :: Point2D -> Point2D -> Point2D -> Direction 
direction (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3)
    | crossProd == 0 = Straight
    | crossProd > 0 = Left
    | crossProd < 0 = Right
    where crossProd = (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)

-- 11 ---------------------------------------------------------------------
directions :: [Point2D] -> [Direction]
directions xs
    | length xs < 3 = []
directions (x:xs@(y:z:_)) = (direction x y z):directions xs

-- 12 ---------------------------------------------------------------------
-- don't work correctly...
grahamScan :: [Point2D] -> [Point2D]
grahamScan xs | length xs < 3 = []
grahamScan xs = scan sortedPoints
    where p0 = foldl1 (\acc v -> if y v < y acc ||
                                   (y v == y acc && x v < x acc) then v else acc) xs
          --
          sortedPoints = sortBy (cmpDir p0) xs
          --
          cmpDir :: Point2D -> Point2D -> Point2D -> Ordering
          cmpDir a b c = case (direction a b c) of
              Left     -> LT
              Right    -> GT
              Straight -> EQ
          --
          scan (x:xs@(y:z:_)) = if (direction x y z) == Left 
                                      then x:grahamScan xs
                                      else grahamScan xs
                


-- 2 ----------------------------------------------------------------------
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = [[]]
splitWith _ (x:[]) = [[x]]
splitWith p (x:xs) = if p x
    then [x]:splitWith p xs
    else let r = splitWith p xs
             h = head r
             t = tail r
         in (x:h):t

splitWith' :: (a -> Bool) -> [a] -> [[a]]
splitWith' _ [] = [[]]
splitWith' _ (x:[]) = [[x]]
splitWith' p (x:xs)
    | p x == True = [x]:splitWith' p xs
    | otherwise   = (x:h):t 
        where r = splitWith' p xs
              h = head r
              t = tail r
