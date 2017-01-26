import System.Environment
import Data.Char

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
            | NUM   Int
            | ADD   String
            | SUB   String
            | MUL   String
            | DIV   String
            | LPAR  String
            | RPAR  String
            | SEMICOLON String
            | PRINT String
            | EQUALS String
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
findCommentEnd' count [] = []
findCommentEnd' count (x:xs) = case x of 
                    "*/" -> if ((count-1) == 0) then xs else findCommentEnd' (count-1) xs -- found a close. if count-1==0, we are don, else keep recursing with count-1
                    "/*" -> findCommentEnd' (count+1) xs    -- found a new nested block, increase count and recurse
                    _    -> findCommentEnd' count xs    -- contained in block recurse on count

                    
-- checks to see if a string qualifies as an id                    
isID :: [Char] -> Bool
isID (x:xs) = if (x `elem` (['a'..'z']++['A'..'Z']++"_"))
                then 
                    if (xs == [])
                    then True
                    else isID' xs 
                else False
                
isID' :: [Char] -> Bool 
isID' (x:xs) = if (x `elem` (['a'..'z']++['A'..'Z']++"_")++['0'..'9'])
                then 
                    if (xs == [])
                    then True
                    else isID' xs 
                else False           

-- checks to see if a string can be a digit
isNum :: [Char] -> Bool
isNum (x:xs) =    if (isDigit x)
                    then
                        if (xs == [])
                        then True
                        else isNum xs
                    else False                    
             

-- look through the entire string. if the character is found,              
findSymbols :: String -> [String]
findSymbols [] = []
findSymbols (x:[]) = [[x]]
findSymbols (x:y:xs) =  if ([x,y] `elem` [":=", "/*", "*/"])
                        then [x,y]:(findSymbols xs)
                        else
                            if (x `elem` [';','+','-','*', '/','\'','(',')'])
                            then [x]:(findSymbols (y:xs))
                            else merge x (findSymbols (y:xs))    
                    
merge :: Char -> [String] -> [String]
merge x [] = [[x]]
merge x (y:ys) = if (y `elem` [";","+","-","*","/","\'","(",")",":=","/*","*/"])
                then [x]:[y]++ys
                else [x:y]++ys
                
                
strToInt :: String -> Int
strToInt [] = 0
strToInt x = strToInt' x 0

strToInt' :: String -> Int -> Int
strToInt' [] _ = 0
strToInt' (x:xs) c = ((digitToInt x) * (10^c)) + (strToInt' xs (c+1))




-- look for a line comment symbol in the string. if found ignore it and everything after it                    
findLineComment :: String -> String
findLineComment [] = []
findLineComment (x:xs) = if (x == '%') then [] else x:(findLineComment xs)


asdf :: String -> [String]
asdf str =  concat $
                map findSymbols $
                    concat $ 
                        map words $
                            map findLineComment $ 
                                lines str            


-- lexer function takes a string and returns a Token list.
lexer :: String ->[Token]
lexer str = lexer' $ 
                concat $
                    map findSymbols $
                        concat $ 
                            map words $
                                map findLineComment $ 
                                    lines str

lexer' :: [String] -> [Token]
lexer' [] = []
lexer' (x:xs)
    | x == []       = []
    | x == "if"     = (IF x)        : lexer' xs 
    | x == "then"   = (THEN x)      : lexer' xs
    | x == "while"  = (WHILE x)     : lexer' xs
    | x == "do"     = (DO x)        : lexer' xs
    | x == "input"  = (INPUT x)     : lexer' xs
    | x == "else"   = (ELSE x)      : lexer' xs
    | x == "begin"  = (BEGIN x)     : lexer' xs
    | x == "end"    = (END x)       : lexer' xs
    | x == "write"  = (WRITE x)     : lexer' xs
    | x == "print"  = (PRINT (head xs))     : lexer' (tail xs)     -- added this since it is obviously a function
    | x == ":="     = (EQUALS x)   : lexer' xs
    | x == "+"      = (ADD x)       : lexer' xs
    | x == "-"      = (SUB x)       : lexer' xs
    | x == "*"      = (MUL x)       : lexer' xs
    | x == "/*"     = lexer' $ findCommentEnd xs
    | x == "/"      = (DIV x)       : lexer' xs
    | x == "("      = (LPAR x)      : lexer' xs
    | x == ")"      = (RPAR x)      : lexer' xs
    | x == ";"      = (SEMICOLON x) : lexer' xs
    -- | x == "%"   = lexer' (findNewLine xs)    -- removed since this is when splitting by shitespace
    -- | x == "/*"     = lexer' (findCommentEnd xs)
    | isID x        = (ID x)        : lexer' xs
    | isNum x       = (NUM $ strToInt x)    : lexer' xs
    | otherwise     = (ERROR (x++"------------------------------------------"))     : lexer' xs


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
            --mapM putStrLn (asdf conts)
            let tokens = lexer conts 
            putStrLn "TOKENS:\n==============================================\n"
            mapM_ (putStrLn.show) tokens
            putStrLn "\n==============================================\nEND TEST"

