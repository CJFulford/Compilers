{-
    Author:     Cody Fulford
    Student ID: 10108658
    Date:       1-17-2017
    Purpose:    First assignment for CPSC 411 - Compilers at the University of Calgary, Winter 2017.
-}

{-
    Your task is to implement a compiler to "basic stack machine code" (described below) for the language M- whose syntax is given by the grammar below. 
    For part  #1 of the assignment you must create a lexical analyzer using LEX to produce a list of tokens (which you should print as a space separated list of just token names).  
    
    Minisculus grammar: 
    prog -> stmt. 
    stmt -> IF expr THEN stmt ELSE stmt
                | WHILE expr DO stmt
                | INPUT ID
                | ID ASSIGN expr
                | WRITE expr
                | BEGIN stmtlist END. 
    stmtlist -> stmtlist stmt SEMICOLON
                |. 
    expr -> expr addop term 
                | term. 
    addop -> ADD
                | SUB. 
    term -> term mulop factor 
                | factor. 
    mulop -> MUL
                | DIV. 
    factor -> LPAR expr RPAR
                | ID
                | NUM
                | SUB NUM.


    This grammar has left recursive nonterminals.  The grammar should be transformed to remove the left recursion and whence into a grammar suitable for recursive descent parsing before implementing it.

    Where the tokens above stand for the following lexemes: 

    "if" => IF 
    "then" => THEN 
    "while" => WHILE
    "do" => DO 
    "input" => INPUT
    "else" => ELSE 
    "begin" => BEGIN 
    "end" => END 
    "write" => WRITE
    {alpha}[{digit}{alpha}]* => ID (identifier) 
    {digit}+ => NUM (positive integer) 
    "+" => ADD 
    ":=" => ASSIGN
    "-" => SUB 
    "*" => MUL
    "/" => DIV
    "(" => LPAR
    ")" => RPAR
    ";"=> SEMICOLON

    Minisculus has two types of comments: 
    (recursive) multi-line comments:        /* comment */ and one-line comments:      % comment 
    The two sorts of comment interact!  One-line comments should take precedence over the multi-line comments.  Thus, the eventual behavior should be equivalent to first stripping out the one-line comments and then removing the (possibly nested) multi-line comments.
-}

import System.Environment   
import Data.List 

main :: IO ()
main = do
    args <- getArgs
    
    -- State the test code that this execution is running since this code is run multiple times
    putStrLn "=================================================="
    putStr   "Running test program: "
    print args
    putStrLn "=================================================="
    
    -- all this code does is print the contents of the testing file
    case args of 
            [file] -> do
                x <- readFile file
                putStr x
            _ -> putStrLn "Wrong number of arguments"
    
    
    
    putStrLn "\n=================================================="
    putStrLn "Program Complete"
      
    
    
    
    
    

    
    
    
    
    
    
    
    