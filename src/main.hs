import System.Environment

data Token =  IF    String
            | THEN  String
            | WHILE String
            | DO    String
            | INPUT String
            | ELSE  String
            | BEGIN String
            | END   String 
            | WRITE String
            | ID    String
            | NUM   String
            | ADD   String
            | SUB   String
            | MUL   String
            | DIV   String
            | LPAR  String
            | RPAR  String
            | SEMICOLON String
            | ERROR String
            deriving (Eq,Show)
            
-- found beginning of line comment, ignore everything until after the next new line            
findNewLine :: [String] -> [String]
findNewLine [] = []
findNewLine (x:xs) = case x of 
            "\n" -> xs
            _    -> findNewLine xs

-- found start of block comment. start helper function with initial count of 1, as we have 1 nestyed block            
findCommentEnd :: [String] -> [String]
findCommentEnd [] = []
findCommentEnd x = findCommentEnd' 1 x

-- helper function. takes in number of nexted block comments and ignores everything untill all nested blocks are closed
findCommentEnd' :: Int -> [String] -> [String]
findCommentEnd' c [] = []
findCommentEnd' count (x:xs) = case x of 
                    "*/" -> if ((count-1) == 0) then xs else findCommentEnd' (count-1) xs -- found a close. if count-1==0, we are don, else keep recursing with count-1
                    "/*" -> findCommentEnd' (count+1) xs    -- found a new nested block, increase count and recurse
                    _    -> findCommentEnd' count xs    -- contained in block recurse on count

-- checks to see if a string qualifies as an id                    
isID :: [Char] -> Bool
isID (x:_) = if (x `elem` (['a'..'z']++['A'..'Z'])) then True else False

-- checks to see if a string can be a digit
isDigit :: [Char] -> Bool
isDigit (x:xs) =    if (x `elem` ['0'..'9'])
                    then
                        if (xs == [])
                        then True
                        else isDigit xs
                    else False
                    
findSemicolons :: String -> [String]
findSemicolons x =  if (last x == ';')
                    then [(init x), [last x]]
                    else [x]
                    
findLineComment :: String -> String
findLineComment [] = []
findLineComment (x:xs) = if (x == '%') then [] else x:(findLineComment xs)            

-- lexer function takes a string and returns a Token list.
lexer :: String ->[Token]
lexer str = lexer' (concat(map findSemicolons(
                        concat(map words (map findLineComment(lines str))))))

lexer' :: [String] -> [Token]
lexer' [] = []
lexer' (x:xs) = case x of 
    "if"    -> (IF x)       : lexer' xs 
    "then"  -> (THEN x)     : lexer' xs
    "while" -> (WHILE x)    : lexer' xs
    "do"    -> (DO x)       : lexer' xs
    "input" -> (INPUT x)    : lexer' xs
    "else"  -> (ELSE x)     : lexer' xs
    "begin" -> (BEGIN x)    : lexer' xs
    "end"   -> (END x)      : lexer' xs
    "write" -> (WRITE x)    : lexer' xs
    "+"     -> (ADD x)      : lexer' xs
    "-"     -> (SUB x)      : lexer' xs
    "*"     -> (MUL x)      : lexer' xs
    "/"     -> (DIV x)      : lexer' xs
    "("     -> (LPAR x)     : lexer' xs
    " )"    -> (RPAR x)     : lexer' xs
    ";"     -> (SEMICOLON x): lexer' xs
    "%"     -> lexer' (findNewLine xs)
    "/*"    -> lexer' (findCommentEnd xs)
    isID    -> (ID x)       : lexer' xs
    isDigit -> (NUM x)      : lexer' xs
    _       -> lexer' xs


main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./eng_lang inputfile > got < ./eng_lang >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            putStrLn fname
            conts <- readFile fname
            let tokens = lexer conts 
            putStrLn "TOKENS:\n==============================================\n"
            mapM_ (putStrLn.show) tokens
            putStrLn "\n==============================================\nEND TEST"

