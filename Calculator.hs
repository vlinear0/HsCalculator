module Calculator where

import Data.Char (isDigit)




data Tokens = Number Float
            | Open
            | Close
            | Exp Char
            | Eof
            deriving (Show, Read, Eq);


isExpr :: Char -> Bool
isExpr = flip elem "+-*/";


parse :: String -> [Tokens]
parse [] = [Eof]
parse ('(':xs) = Open:parse xs
parse (')':xs) = Close:parse xs
parse (x:xs)
  | isDigit x || x == '.' = let (num, rest) = span (\ch -> isDigit ch || ch == '.') (x:xs)
                            in Number (read num):parse rest
  | isExpr x = Exp x:parse xs
  | x == ' ' = parse xs
  | otherwise = parse xs