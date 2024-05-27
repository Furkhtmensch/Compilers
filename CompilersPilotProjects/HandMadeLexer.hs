import Data.Char (isDigit, isAlpha, isAlphaNum)

data Token = ID [Char]
           | NUM Int
           | ARIT [Char]
           | IF
           | RETURN
           | LPAREN
           | RPAREN
           | LBRACK
           | RBRACK
           | COMMA
           | SEMI
           | COMPEQ
           | EOF
    deriving (Show, Eq)

lexer :: [Char] -> [Token]
lexer ""        = EOF:[]
lexer ('\n':xs) = lexer xs
lexer (' ':xs)  = lexer xs
lexer ('(':xs)  = LPAREN:lexer xs
lexer (')':xs)  = RPAREN:lexer xs
lexer ('{':xs)  = LBRACK:lexer xs
lexer ('}':xs)  = RBRACK:lexer xs
lexer (',':xs)  = COMMA:lexer xs
lexer (';':xs)  = SEMI:lexer xs
lexer (x:y:xs) | isArit x && isArit' y = ARIT [x,y]:lexer xs
               | isArit x              = ARIT [x]:lexer (y:xs)
lexer (x:xs) | isDigit x = lexerNum (x:xs'):lexer xs''
                where xs'  = takeWhile (isDigit) xs
                      xs'' = dropWhile (isDigit) xs
lexer (x:xs) | isAlpha x = lexerText (x:xs'):lexer xs''
                where xs'  = takeWhile (isAlphaNum) xs
                      xs'' = dropWhile (isAlphaNum) xs
lexer (x:_) = error ("Invalid character: " ++ show x)

isArit :: Char -> Bool
isArit x = any (x ==) ['=', '&', '|', '+', '-', '*', '/', '>', '<', '%', '^']

isArit' :: Char -> Bool
isArit' x = any (x ==) ['=', '&', '|', '+', '-']

lexerNum :: [Char] -> Token
lexerNum xs = NUM (read xs)

lexerText :: [Char] -> Token
lexerText "if"     = IF
lexerText "return" = RETURN
lexerText xs       = ID xs

getLexer :: IO ()
getLexer = do txt <- getContents
              print (lexer txt)

main = getLexer
