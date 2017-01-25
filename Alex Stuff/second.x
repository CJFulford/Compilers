{
module Main (main) where
import System.Environment
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+               ;
  "--".*                ;
  let                   { \p s -> Let p}
  in                    { \p s -> In p }
  $digit+               { \p s -> Int (read s) p }
  [\=\+\-\*\/\(\)]          { \p s -> Sym (head s) p }
  $alpha [$alpha $digit \_ \']*     { \p s -> Var s p }

{
-- Each action has type :: String -> Token

-- data AlexPosn = AlexPosn Int Int Int 
-- The token type:
data Token =
      Let AlexPosn        
    | In AlexPosn
    | Sym Char AlexPosn  
    | Var String AlexPosn 
    | Int Int AlexPosn
    deriving (Eq,Show)

main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./eng_lang inputfile > got < ./eng_lang >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            conts <- readFile fname
            let tokens = alexScanTokens conts 
            putStrLn "\n**************************************\n"
            putStrLn "The List of tokens are as follows.\n"
            mapM_ (putStrLn.show) tokens
}

