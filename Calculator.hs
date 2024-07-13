module Calculator where


import Data.Char (isDigit)



data Op = Inc
        | Dec
        | Mul
        | Div
        deriving (Show);

data Tokens = Number Float
            | Open
            | Close
            | Exp Op
            | Eof
            deriving (Show);

data AST = NumNode Float
         | OpNode Op AST AST
         deriving (Show);



isExpr :: Char -> Bool
isExpr = flip elem "+-*/";


getOp :: Char -> Op
getOp x = case x of
  '+' -> Inc
  '-' -> Dec
  '*' -> Mul
  '/' -> Div
  _   -> error "Unexpected error while parsing operation.";


lexer :: String -> [Tokens]
lexer ('(':xs) = Open:lexer xs
lexer (')':xs) = Close:lexer xs
lexer (x:xs)
  | isDigit x || x == '.' = let (num, rest) = span (\ch -> isDigit ch || ch == '.') (x:xs)
                            in Number (read num):lexer rest
  | isExpr x = Exp (getOp x):lexer xs
  | x == ' ' = lexer xs
  | otherwise = lexer xs;
lexer [] = [Eof]