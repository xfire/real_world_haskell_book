lastButOne :: [a] -> Maybe a
lastButOne (x:y:[]) = Just(x)
lastButOne (x:y:xs) = lastButOne (y:xs)
lastButOne _ = Nothing

lastButOne' :: [a] -> Maybe a
lastButOne' a@ (x:y:xs) = Just(last . init $ a)
lastButOne' _ = Nothing
