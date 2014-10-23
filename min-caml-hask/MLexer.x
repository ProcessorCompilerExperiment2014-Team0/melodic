{
{-# OPTIONS_GHC -w #-}
module MLexer where

import Prelude hiding (EQ, LT, GT)
import Id

}
%wrapper "basic"


$space = [\ \t \n \r]
$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$lower = a-z
$upper = A-Z
$unknown = [\x00-\x10ffff] # $space # $digit # $alpha # [\(\*\)\+\-\/\=\<\>\,\_\;]

tokens :-
  $space+;
  "(*" { \_ -> COMM_BEGIN }
  "*)" { \_ -> COMM_END }
  "(" { \_ -> LPAREN }
  ")" { \_ -> RPAREN }
  "true" { \_ -> BOOL True }
  "false" { \_ -> BOOL False }
  "not"  { \_ -> NOT }
  $digit+ { \s -> INT (read s) }
  $digit+ \. ($digit*)? ([eE] ([\+\-]?) $digit+)? { \s -> FLOAT (readFloat s) }
  "-" { \_ -> MINUS }
  "+" { \_ -> PLUS }
  "*" { \_ -> AST }
  "/" { \_ -> SLASH }
  "-." { \_ -> MINUS_DOT }
  "+." { \_ -> PLUS_DOT }
  "*." { \_ -> AST_DOT }
  "/." { \_ -> SLASH_DOT }
  "="  { \_ -> EQ }
  "<>"  { \_ -> NEQ }
  "<="  { \_ -> LE }
  ">="  { \_ -> GE }
  "<"  { \_ -> LT }
  ">"  { \_ -> GT }
  "if"  { \_ -> IF }
  "else"  { \_ -> THEN }
  "then"  { \_ -> ELSE }
  "let"  { \_ -> LET }
  "in"  { \_ -> IN }
  "rec"  { \_ -> REC }
  ","   { \_ -> COMMA }
  "_"  { \_ -> PLACEHOLDER }
  "Array.create"  { \_ -> ARRAY_CREATE }
  "."   { \_ -> DOT }
  "<-"   { \_ -> LESS_MINUS }
  ";"   { \_ -> SEMICOLON }
  $lower ($digit|$lower|$upper|"\_")* { \x -> ID (Id x) }
  $unknown+  { UNKNOWN }

{
data Token
  = BOOL !Bool | INT !Int | FLOAT !Float
  | NOT | MINUS | PLUS | AST | SLASH | MINUS_DOT | PLUS_DOT | AST_DOT | SLASH_DOT | EQ | NEQ | LE | GE | LT | GT | IF | THEN | ELSE | ID Id
  | LET | IN | REC | COMMA | ARRAY_CREATE | DOT | LESS_MINUS | SEMICOLON | LPAREN | RPAREN | EOF
  | COMM_BEGIN | COMM_END -- comment begin/end
  | PLACEHOLDER -- Instead of using identifier generated by Id.gentmp, we use distinct constructor.
  | UNKNOWN !String
  deriving (Eq, Show)

readFloat :: String -> Float
readFloat s
  | s !! (length s - 1) == '.' = read $ s ++ "0"
  | otherwise = read s

lex :: String -> [Token]
lex s = alexScanTokens s ++ [EOF]
}
