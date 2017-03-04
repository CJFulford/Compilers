{-
    Author: Cody Fulford
    Purpose: Assignment 2 submission for CPSC 411 at the University of Calgary, W17
    Notes:
        majority of the code has been written by myself.
        Main, lexer, and lexer' funcations have been based on The same functions in "eng_lang.hs" by Prashant Kumar
-}

{-
    Here is the grammar that i am using 
    
    prog -> IF expr thenpart
            | WHILE expr dopart
            | INPUT ID
            | ID ASSIGN expr
            | WRITE expr
            | PRINT stmtlist                I added this since it was not contained in the original grammar and needed to be added
            | BEGIN stmtlist endpart
            
    stmtlist -> IF expr thenpart semipart stmtlist
                | WHILE empr dopart semipart stmtlist
                | INPUT ID semipart stmtlist
                | ID ASSIGN empr semipart stmtlist
                | WRITE expr semipart
                | PRINT expr semipart
                | BEGIN stmtlist endpart semipart stmtlist
                | .
                
    expr -> term expr'
    
    expr' -> ADD term expr'
            | SUB term expr'
            | .
            
    term -> factor term'
    
    term' -> MUL factor term'
            | DIV factor term'
            | .
            
    factor -> LPAR expr rpart
            | ID
            | NUM
            | SUB NUM
            
    thenpart    -> THEN prog elsepart
    elsepart    -> ELSE prog
    dopart      -> DO prog
    endpart     -> END
    semipart    -> SEMICOLON
    rpart       -> RPAR

    
-}
import Text.PrettyPrint.GenericPretty
import System.Environment
import Data.Char

-- variables that are used through out the code. easier to define them here.
numRange = ['0'..'9']
alphaRange = ['a'..'z']++['A'..'Z']
operators = [';','+','-','*', '/','\'','(',')']
doubleOperators = [":=", "**", "/*", "*/"]
strOperators = [";","+","-","*", "/","\'","(",")", ":=", "**", "/*", "*/"]

data Token =  T_IF    String
            | T_THEN  String
            | T_WHILE String
            | T_DO    String
            | T_INPUT String
            | T_ELSE  String
            | T_BEGIN String
            | T_END   String 
            | T_WRITE String
            | T_ID    String
            | T_NUM   Int
            | T_ADD   String
            | T_SUB   String
            | T_MUL   String
            | T_DIV   String
            | T_LPAR  String
            | T_RPAR  String
            | T_SEMICOLON String
            | T_PRINT String  -- added this
            | T_ASSIGN String -- added this
            | T_ERROR String  -- added this
            deriving (Eq,Show)
          
            
-- Syntax Tree, we need to derive generic and out in order to user the generic pretty print       
data Prog = If Expr Prog Prog
            | While Expr Prog
            | Input String
            | Id String Expr
            | Write Expr
            | Print Expr
            | Begin Stmtlist
            deriving (Out, Generic)
data Stmtlist = S_If Expr Prog Prog Stmtlist
            | S_While Expr Prog Stmtlist
            | S_Input String Stmtlist
            | S_Id String Expr Stmtlist
            | S_Write Expr
            | S_Print Expr
            | S_Begin Stmtlist Stmtlist
            | Svoid
            deriving (Out, Generic)
data Expr = Expr Term Expr'
            deriving (Out, Generic)
data Expr' = Add Term Expr'
            | Sub Term Expr'
            | Evoid
            deriving (Out, Generic)
data Term = Term Factor Term'
            deriving (Out, Generic)
data Term' = Mul Factor Term'
            | Div Factor Term'
            | Tvoid
            deriving (Out, Generic)
data Factor = Lpar Expr
            | F_Id String
            | Num Int
            | Neg Int
            deriving (Out, Generic)  
            
            
--instance (Out a) => Out (Prog a)
            
            
main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               putStrLn "INCORRECT NUMBER OF ARGUMENTS, THERE SHOULD BE EXACTLY 1 OPERATOR WHICH IS THE PATH TO THE m- FILE TO RUN"
        False -> do
            let fname  = args !! 0 
            putStrLn fname
            conts <- readFile fname
            
            
            let tokens = lexer conts 
            
            putStrLn "TOKENS:\n\n"
            --mapM_ (putStrLn.show) tokens
            
            
            let tree = parser tokens
            
            putStrLn "\n======================\nTREE:\n\n"
            pp tree -- pretty print the tree
            
            
            putStrLn "\n==============================================\nEND TEST"
            
            
-- recieves the input of the file as a single large string.
-- recieves the input of the file as a single large string.
-- splits the string into lines, removes any line comments
-- splits the lines into words
-- analysys the words for symbols to create more words
-- creates a large list of strings and runs lexer' which transforms them into tokens
lexer :: String ->[Token]
lexer input = lexer' $ 
                concat $
                    map findSymbols $
                        concat $ 
                            map words $
                                map cutLineComment $ 
                                    lines input

-- recieves as input a list of strings and converts the stings into tokens
lexer' :: [String] -> [Token]
lexer' [] = []
lexer' (x:xs)
    | x == []       = []
    | x == "begin"  = (T_BEGIN x)     : lexer' xs
    | x == "end"    = (T_END x)       : lexer' xs
    | x == "if"     = (T_IF x)        : lexer' xs 
    | x == "then"   = (T_THEN x)      : lexer' xs
    | x == "while"  = (T_WHILE x)     : lexer' xs
    | x == "do"     = (T_DO x)        : lexer' xs
    | x == "read"   = (T_INPUT x)     : lexer' xs
    | x == "else"   = (T_ELSE x)      : lexer' xs
    | x == "write"  = (T_WRITE x)     : lexer' xs
    | x == "print"  = (T_PRINT x)     : lexer' xs
    | x == ":="     = (T_ASSIGN x)    : lexer' xs
    | x == "+"      = (T_ADD x)       : lexer' xs
    | x == "-"      = (T_SUB x)       : lexer' xs
    | x == "*"      = (T_MUL x)       : lexer' xs
    | x == "/*"     =                 lexer' $ cutBlockComment xs -- line comments (%) have already been handled and are thus not in here
    | x == "/"      = (T_DIV x)       : lexer' xs
    | x == "("      = (T_LPAR x)      : lexer' xs
    | x == ")"      = (T_RPAR x)      : lexer' xs
    | x == ";"      = (T_SEMICOLON x) : lexer' xs
    | isID x        = (T_ID x)        : lexer' xs
    | isNum x       = (T_NUM $ strToInt x)    : lexer' xs
    | canSplit x    =                 lexer' ((splitUnknown x) ++ xs)
    | otherwise     = [T_ERROR x]
     
-- =================================================
-- Parser
     
-- begin the parsing process
parser :: [Token] -> Prog
parser x = y where (y, _) = prog x



-- start the parse program
prog :: [Token] -> (Prog, [Token])
prog ((T_IF _):xs) = (If e p1 p2, end) where 
                        (e, r1) = expr xs
                        (p1,r2) = thenpart r1
                        (p2, end) = elsepart r2
prog ((T_WHILE _):xs) = (While e p1, end) where 
                        (e, r1) = expr xs
                        (p1, end) = dopart r1  
prog ((T_INPUT _):(T_ID x):xs) = (Input x, xs)
prog ((T_ID x):(T_ASSIGN _):xs) = (Id x e, end) where
                            (e, end) = expr xs
prog ((T_WRITE _):xs) = (Write e, end) where
                            (e, end) = expr xs
prog ((T_PRINT _):xs) = (Print e, end) where
                            (e, end) = expr xs
prog ((T_BEGIN _):xs) = (Begin s1, endpart end) where
                            (s1, end) = stmtlist xs



stmtlist :: [Token] -> (Stmtlist, [Token])
stmtlist ((T_IF _):xs) = (S_If e p1 p2 s, end) where
                            (e, r1) = expr xs
                            (p1, r2) = thenpart r1
                            (p2, r3) = elsepart r2
                            (s, end) = stmtlist $ semipart r3
stmtlist ((T_WHILE _):xs) = (S_While e p s, end) where
                            (e, r1) = expr xs
                            (p, r2) = dopart r1
                            (s, end) = stmtlist $ semipart r2
stmtlist ((T_INPUT _):(T_ID x):xs) = (S_Input x s, end) where
                            (s, end) = stmtlist $ semipart xs
stmtlist ((T_ID x):(T_ASSIGN _):xs) = (S_Id x e s, end) where
                            (e, r1) = expr xs
                            (s, end) = stmtlist  $ semipart r1
stmtlist ((T_WRITE _):xs) = (S_Write e, semipart end) where
                            (e, end) = expr xs
stmtlist ((T_PRINT _):xs) = (S_Print e, semipart end) where
                            (e, end) = expr xs
stmtlist ((T_BEGIN _):xs) = (S_Begin s1 s2, end) where
                            (s1, r1) = stmtlist xs
                            (s2, end) = stmtlist $ semipart $ endpart r1
stmtlist x = (Svoid, x)                            

                          
                        
                          
expr :: [Token] -> (Expr, [Token])                        
expr x = (Expr t e, end) where
                        (t, r1) = term x
                        (e, end) = expr' r1

                        
                        
                        
                        
                        
expr' ::  [Token] -> (Expr', [Token])                      
expr' ((T_ADD _):xs) = (Add t e, end) where
                        (t, r1) = term xs
                        (e, end) = expr' r1
expr' ((T_SUB _):xs) = (Sub t e, end) where
                        (t, r1) = term xs
                        (e, end) = expr' r1 
expr' x = (Evoid, x)                        

                        
                        
                        
                        
                        
term :: [Token] -> (Term, [Token])
term x = (Term f t, end) where
                        (f, r1) = factor x
                        (t, end) = term' r1
       




       
term' :: [Token] -> (Term', [Token])
term' ((T_MUL _):xs) = (Mul f t, end) where
                                        (f, r1) = factor xs
                                        (t, end) = term' r1
term' ((T_DIV _):xs) = (Div f t, end) where
                                        (f, r1) = factor xs
                                        (t, end) = term' r1
term' x = (Tvoid, x)                                        
                                        
   




   
factor :: [Token] -> (Factor, [Token])
factor ((T_LPAR _):xs) = (Lpar e, rpart end) where
                            (e, end) = expr xs
factor ((T_ID x):xs) = (F_Id x, xs)
factor ((T_NUM x):xs) = (Num x, xs)
factor ((T_SUB _):(T_NUM x):xs) = (Neg x, xs)








thenpart :: [Token] -> (Prog, [Token])
thenpart ((T_THEN _):xs) = (p, end) where (p, end) = prog xs

elsepart :: [Token] -> (Prog, [Token])
elsepart ((T_ELSE _):xs) = (p, end) where (p, end) = prog xs

dopart :: [Token] -> (Prog, [Token])
dopart ((T_DO _):xs) = (p, end) where (p, end) = prog xs




rpart :: [Token] -> [Token]
rpart ((T_RPAR _):xs) = xs

endpart :: [Token] -> [Token]
endpart ((T_END _):xs) = xs

semipart :: [Token] -> [Token]
semipart ((T_SEMICOLON _):xs) = xs
                                        
                                        
                                        
                                        
-- =================================================
-- Conversion to stack code



tostack :: [Token] -> []


-- =================================================
-- COMMENT HANDLING


-- found beginning of line comment, ignore everything until after the next new line
-- this runs as a pre-processor to the lexer, after 'lines' and before 'words' this ensures that a line comment removes the entire line, and since line comments take presedance against block comments, thi will also remove any block comment open or close after the % on this line                  
cutLineComment :: String -> String
cutLineComment [] = []
cutLineComment (x:xs) = if (x == '%') then [] else x:(cutLineComment xs)

            
-- found start of block comment. start helper function with initial count of 1, as we have 1 nested block            
cutBlockComment :: [String] -> [String]
cutBlockComment [] = ["UNCLOSED BLOCK COMMENT"]
cutBlockComment x = cutBlockComment' 1 x

-- helper function. takes in number of nexted block comments and ignores everything untill all nested blocks are closed
cutBlockComment' :: Int -> [String] -> [String]
cutBlockComment' 0 x = x -- no more nested blocks. return rest of provided string
cutBlockComment' count [] = ["UNCLOSED BLOCK COMMENT"]
cutBlockComment' count (x:xs) = case x of 
                    "*/" -> cutBlockComment' (count-1) xs   -- found a close. decrement count and recurse
                    "/*" -> cutBlockComment' (count+1) xs   -- found a new nested block, increase count and recurse
                    _    -> cutBlockComment' count xs       -- contents of block, keep looking for end

                    
-- =================================================
-- ID HANDLING           


-- checks to see if a string qualifies as an id
-- need secondary function since the first character has a reduced range of valid posibilities                   
isID :: String -> Bool
isID (x:xs)
        | not (x `elem` (alphaRange++"_")) = False  -- first char is not alpha or _, not an ID
        | xs == [] && (x `elem` alphaRange) = True  -- id is  only 1 character and is alpha but not just _, valid ID
        | otherwise = isID' xs                      -- possible id longer than 1 char, move to isID' for large range check
                
-- checks for valid ID, excluding first char
isID' :: String -> Bool
isID' (x:xs) 
        | not (x `elem` (alphaRange++numRange)) = False -- possible ID has characters outside valid range
        | xs == [] = True                               -- no more chars to check, valid ID
        | otherwise = isID' xs                          -- more chars to checks
        
             
-- =================================================
-- SYMBOL HANDLING


-- inputs is a string, one of the strings following word function in lexer
-- output = list of strings, function looks at input string and seperates by symbols "a/b" => ["a", "/", "b"]
findSymbols :: String -> [String]
findSymbols [] = []         -- if at end of string, return what is left
findSymbols (x:[]) = [[x]]  -- if at end of string, return what is left
findSymbols (x:y:xs) 
                | [x,y] `elem` doubleOperators = [x,y]:(findSymbols xs)    -- check for doubleOperators before single as singles can override doubles, seperate double operator from string and recurse on remainder
                | x `elem` operators = [x]:(findSymbols (y:xs)) -- no double found, check for single, if found, separate and recurse on remainder
                | otherwise = merge x (findSymbols (y:xs)) -- no symbols found, use merge

-- attach the char to the front of the first string in string input, unless that string is a symbol                
merge :: Char -> [String] -> [String]
merge x [] = [[x]]
merge x (y:ys)
            | y `elem` strOperators = [x]:y:ys
            | otherwise = [x:y]++ys
            

 
-- =================================================
-- NUMBER HANDLING 


-- checks to see if a string can be a digit
isNum :: String -> Bool
isNum (x:xs)
        | not (isDigit x) = False   -- character is not a digit, cannot be converted to a number, fail
        | xs == [] = True           -- all characters checks, can be converted to number
        | otherwise = isNum xs      -- some chars in string still need to be checked
        
-- convert a string to an int
strToInt :: String -> Int
strToInt [] = 0             -- nothing to convert, return 0
strToInt x = strToInt' x 0  -- need helper method to keep track of place in string

-- input the remainder of string that needs conversion and the number of chars already converted
strToInt' :: String -> Int -> Int
strToInt' [] _ = 0          -- no more to convert, return 0
strToInt' x c = ((digitToInt (last x)) * (10^c)) + (strToInt' (init x) (c+1)) -- grab the last character in the string, convert it to a digit. to correctly add it to the result, multiply it by 10^count, which is where it was in the string, relative to the back. add all of these results together and the number is complete

-- =================================================
-- SPLITING HANDLING 

-- makes sure that the possible splitting string can be made into an id and a num by making sure that all characters are part of the valid set               
canSplit :: String -> Bool
canSplit [] = True
canSplit (x:xs)
        | not (x `elem` (alphaRange++numRange++"_")) = False  -- unknown token. error
        | otherwise = canSplit xs

-- find the first case of a non number in the string then stop recursing and have the number be the first element of the list of strings and the remaining string be the second entry. the second entry remains to be checked if it is a valid ID
splitUnknown :: String -> [String]
splitUnknown [] = []
splitUnknown (x:xs)
                | not (isDigit x) = [[], x:xs]
                | otherwise = [x:y] ++ ys
                    where
                        temp = splitUnknown xs
                        y = head temp
                        ys = tail temp








