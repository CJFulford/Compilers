{-
    Author: Cody Fulford
    Purpose: Assignment 1 submission for CPSC 411 at the University of Calgary, W17
    Notes:
        majority of the code has been written by myself.
        Main, lexer, and lexer' funcations have been based on The same functions in "eng_lang.hs" by Prashant Kumar
-}

import System.Environment
import Data.Char

-- variables that are used through out the code. easier to define them here.
numRange = ['0'..'9']
alphaRange = ['a'..'z']++['A'..'Z']
operators = [';','+','-','*', '/','\'','(',')']
doubleOperators = [":=", "**", "/*", "*/"]
strOperators = [";","+","-","*", "/","\'","(",")", ":=", "**", "/*", "*/"]

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
            | NUM   Int     -- conversion to int does happen
            | ADD   String
            | SUB   String
            | MUL   String
            | DIV   String
            | POW   String  -- added this
            | LPAR  String
            | RPAR  String
            | SEMICOLON String
            | PRINT String  -- added this
            | EQUALS String -- added this
            | ERROR String  -- added this
            deriving (Eq,Show)
            
            
main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               putStrLn "INCORRECT NUMBER OF ARGUMENTS, THERE SHOULD ONLY BE 1 OPERATOR THAT IS THE PATH OF THE m- FILE TO RUN"
        False -> do
            let fname  = args !! 0 
            putStrLn fname
            conts <- readFile fname
            let tokens = lexer conts 
            putStrLn "TOKENS:\n==============================================\n"
            mapM_ (putStrLn.show) tokens
            putStrLn "\n==============================================\nEND TEST"
            
            
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
    | x == ":="     = (EQUALS x)    : lexer' xs
    | x == "+"      = (ADD x)       : lexer' xs
    | x == "-"      = (SUB x)       : lexer' xs
    | x == "**"     = (POW x)       : lexer' xs -- added this since it is in the test code adn i am assuming that it is not an error
    | x == "*"      = (MUL x)       : lexer' xs
    | x == "/*"     =                 lexer' $ cutBlockComment xs -- line comments (%) have already been handled and are thus not in here
    | x == "/"      = (DIV x)       : lexer' xs
    | x == "("      = (LPAR x)      : lexer' xs
    | x == ")"      = (RPAR x)      : lexer' xs
    | x == ";"      = (SEMICOLON x) : lexer' xs
    | isID x        = (ID x)        : lexer' xs
    | isNum x       = (NUM $ strToInt x)    : lexer' xs
    | otherwise     = (ERROR x)     : lexer' xs
    
    
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
isID :: [Char] -> Bool
isID (x:xs)
        | not (x `elem` (alphaRange++"_")) = False  -- first char is not alpha or _, not an ID
        | xs == [] && (x `elem` alphaRange) = True  -- id is  only 1 character and is alpha but not just _, valid ID
        | otherwise = isID' xs                      -- possible id longer than 1 char, move to isID' for large range check
                
-- checks for valid ID, excluding first char
isID' :: [Char] -> Bool
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
isNum :: [Char] -> Bool
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

