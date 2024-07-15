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


isNum :: Char -> Bool
isNum x = isDigit x || x == '.';


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
  | x == ' ' = lexer xs
  | isDigit x || x == '.' = let (num, rest) = span (isNum) (x:xs)
                            in Number (read num):lexer rest
  | isExpr x = Exp (getOp x):lexer xs
  | otherwise = lexer xs;
lexer [] = [Eof]


parseToAst :: String -> AST
parseToAst t =
  let (ast, rest) = parseExpr $ lexer t
  in case rest of
    [Eof] -> ast
    _     -> error "Unexpected error while parsing expression: ";


parseExpr :: [Tokens] -> (AST, [Tokens])
parseExpr t =
  let (term, rest) = parseTerm t
  in parseExpr' term rest


parseTerm :: [Tokens] -> (AST, [Tokens])
parseTerm t =
  let (fac, rest) = parseFac t
  in parseTerm' fac rest


parseFac :: [Tokens] -> (AST, [Tokens])
parseFac (Number t:ts) = (NumNode t, ts)
parseFac (Open:ts) = let (ast, rest) = parseExpr ts
                     in case rest of
                      (Close:ts') -> (ast, ts')
                      _           -> error "Unclosed branch!"
parseFac ts = error $ "Exception while parsing next tokens: " ++ show ts;



parseTerm' :: AST -> [Tokens] -> (AST, [Tokens])
parseTerm' lhs (Exp Mul:ts) =
  let (rhs, rest) = parseFac ts
      nLhs = OpNode Mul lhs rhs
  in parseTerm' nLhs rest
parseTerm' lhs (Exp Div:ts) =
  let (rhs, rest) = parseFac ts
      nLhs = OpNode Div lhs rhs
  in parseTerm' nLhs rest
parseTerm' lhs ts = (lhs, ts)


parseExpr' :: AST -> [Tokens] -> (AST, [Tokens])
parseExpr' lhs (Exp o:ts) =
  let (rhs, rest) = parseTerm ts
      nLhs = OpNode o lhs rhs
  in parseExpr' nLhs rest
parseExpr' ast ts = (ast, ts)




{-
    Процесс работы програмы на примере "2 + 2":

    1. Вызываем parseToAst "2 + 2"
    2. Получаем массив токенов которые распарсил lexer    -> [Number 2.0, Exp Inc, Number 2.0]
    3. Функция вызывает внутри себя parseExpr  ------
    4. parseExpr вызывает внутри себя parseTerm -------
    5. parseTerm вызывает внутри себя parseFac -------
    6. parseFac возвращает вот такое преобразование [Number 2.0, Exp Inc, Number 2.0] -> (NumNode 2.0, [Exp Inc, Number 2.0])
    7. parseTerm получил внутри себя (NumNode 2.0, [Exp Inc, Number 2.0]) и вызывает parseTerm'
    8. parseTerm' обрабатывает математические операции умножения и деления и решает как складывать данные в дереве
       Если таковых операций нет - возыращает все в том же виде в котором данные пришли в функцию -> (NumNode 2.0, [Exp Inc, Number 2.0])
    9. Данные приходят в parseExpr которая вызывает parseExpr'
    10. parseExpr' принимает дерево и массив токенов. Если токен - операция обрабатывает его и вставляет в дерево, иначе - в примере ошибка
        вызываем parseTerm с [Exp Inc, Number 2.0], -> (NumNode 2.0)
    11. parseExpr' создает новый узел OpNode и возвращает саму себя рекурсивно для проверки есть ли далее еще операторы которые можно обработать
-}