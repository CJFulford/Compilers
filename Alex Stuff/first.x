{
module Main (main) where
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
data Token =
    Let         |
    In          |
    Sym Char    |
    Var String  |
    Int Int
    deriving (Eq,Show)


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

