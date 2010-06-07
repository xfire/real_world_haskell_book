import Data.Char (isDigit, digitToInt)

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

-- folds --
-- 1 ----------------------------------------------------------------------
asInt_fold :: String -> Int
asInt_fold ('-':xs) = negate $ asInt_fold xs
asInt_fold xs = foldl (\acc v -> acc * 10 + digitToInt v) 0 xs

asInt_fold' :: String -> Int
asInt_fold' ('-':xs) = negate $ asInt_fold' xs
asInt_fold' xs = foldl ((+) . (* 10)) 0 $ map digitToInt xs

asInt_either :: String -> Either String Int
asInt_either ('-':xs) = case asInt_either xs of
                            Left msg -> Left msg
                            Right v -> Right $ negate v
asInt_either xs = foldl compute (Right 0) xs
    where compute (Left  msg) _ = Left msg
          compute (Right acc) c
                    | isDigit c = Right $ acc * 10 + digitToInt c
                    | otherwise = Left $ "non-digit " ++ [c]

concat' :: [[a]] -> [a]
concat' = foldr (++) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile'     _ [] = []
takeWhile' f (x:xs) = case f x of
    True  -> x:(takeWhile' f xs)
    False -> []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f = foldr (\v acc -> if f v then v:acc else acc) []

-- groupBy' (\a b -> (a > 0) == (b > 0)) [-2, -1, 0, 1, 2, 3, -2, -1]
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr step []
    where step v [] = [[v]]
          step v acc
            | f v (head $ head acc) = (v:head acc):tail acc
            |             otherwise = [v]:acc
