{
import System.Environment
}

%wrapper "basic"

$digit   = 0-9            -- digits
$alpha   = [a-zA-Z]       -- alphabetic characters
$newline = [\n]
@symbols = [\=\+\-\*\/\(\)] 


tokens :-

  $white+                         ;
  "--".* $newline+                ;
  $newline                        ;
  "{-" ([.]* $newline)+ "-}"      ;
  let                   { \s -> Let }
  in                    { \s -> In }
  $digit+               { \s -> Int (read s) }
  @symbols              { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*     { \s -> Var s }

{
-- Each action has type :: String -> Token


-- The token type:
data Token = IF
            | THEN
            | WHILE
            | Do
            | INPUT
            | ELSE
            | BEGIN
            | END
            | WRITE
            | ID $alpha
            | NUM $digit+
            | ADD
            | ASSIGN
            | SUB
            | MUL
            | DIV
            | LPAR
            | RPAR
            | SEMICOLON
    deriving (Eq,Show)

    -- lexer function takes a string and returns a Token list.
lexer :: String ->[Token] 
lexer str = lexer' inps 
              where  
                alllines = lines str 
                                  -- lines takes a string and converts 
                                  -- it into a list of lines based on
                                  -- separation by '\n'
                strs = concat $ map words alllines
                                  -- words takes a string and converts 
                                  -- it into a list of words based on 
                                  -- separation by spaces  
                inps = stringtoStrings strs -- some other magic


stringtoStrings ::  [String] -> [String]
stringtoStrings [] = []
stringtoStrings (x:xs) = case elem ',' x of
    True  -> (split (dropBlanks $ oneOf ",") x ) ++ stringtoStrings xs   
    False -> case elem '.' x of 
        True  -> (split (dropBlanks $ oneOf ".") x ) ++ stringtoStrings xs
        False ->  x: stringtoStrings xs




lexer' :: [String] -> [Token]
lexer' [] = []
lexer' (x:xs) 
    | x == "I"                      = (Pronoun     x) : lexer' xs 
    | elem (toLower' x) pronoun     = (Pronoun     x) : lexer' xs 
    | elem (toLower' x) verb        = (Verb        x) : lexer' xs 
    | elem (toLower' x) adverb      = (Adverb      x) : lexer' xs 
    | elem (toLower' x) preposition = (Preposition x) : lexer' xs 
    | elem (toLower' x) conjuction  = (Conjuction  x) : lexer' xs 
    | elem (toLower' x) adjective   = (Adjective   x) : lexer' xs 
    | elem (toLower' x) article     = (Article     x) : lexer' xs  
    | x == "."                      = FStop           : lexer' xs 
    | x == ","                      = Comma           : lexer' xs 
    | last x == '.'                 = lexer' (init x:".":xs)
    | isUpper(head x)               = (Noun x) : lexer' xs 
    | otherwise                     = error "\n****************Error: Got a non Token. :(\n"
    where 
        -- toLower' is a function which converts takes  a string and 
        -- if the first character of the string is a capital letter then it changes 
        -- it to lower case (rest is unchanged) otherwise returns the
        -- original string.
        toLower' :: String -> String
        toLower' str@(x:xs) = case isLower x of
            True  -> str 
            False -> (toLower x): xs 

main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./Main inputfile > got < ./Main >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            conts <- readFile fname
            let tokens = alexScanTokens conts 
            putStrLn "\n**************************************\n"
            putStrLn "The List of tokens are as follows.\n"
            mapM_ (putStrLn.show) tokens
}

