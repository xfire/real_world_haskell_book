firstWord :: String -> String
firstWord input = unlines . map (head . words) $ lines input

main = interact firstWord
