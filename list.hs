data List a = Cons a (List a)
            | Nil
              deriving Show

toList :: [a] -> List a
toList = foldr Cons Nil

fromList :: List a -> [a]
fromList Nil = []
fromList (x `Cons` xs) = x : fromList xs
