
{
module Lexer where
}

%wrapper "basic"

$digit = [0-9]          -- digits
$nonZeroDigit = [1-9]   -- digits > 0
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-   
    $white+     ;    
    "p"         {\s -> TokenP}
    "cnf"       {\s -> TokenCNF}
    "c ".*      ; -- Comment line
    "e"         {\s -> TokenE}
    "a"         {\s -> TokenA}
    "0"         {\s -> TokenZero}
    "-"         {\s -> TokenMinus}
    $nonZeroDigit $digit*   {\s -> TokenInt (read s)}
    --$digit+          {\s -> TokenInt (read s)}


{
    
data Token = TokenP
            | TokenCNF
            | TokenE            
            | TokenA
            | TokenZero
            | TokenMinus
            | TokenInt Int            
            deriving (Eq, Show)


test = do
    s <- getLine
    print (alexScanTokens s)
}