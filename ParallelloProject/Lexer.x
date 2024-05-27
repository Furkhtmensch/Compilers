{
module Lexer where
}
%wrapper "posn"
$alpha = [_a-zA-Z]
$digit = [0-9]
$arit = [\+\*\-\/\%]
$comp = [\<\=\>!]
tokens :-

$white+                 ;
$comp"="|$comp          { \pos s -> COMP pos s }
$arit                   { \pos s -> ARIT pos s }
$digit+                 { \pos s -> NUM pos (read s) }
$digit+"."$digit+       { \pos s -> REAL pos (read s) }
{                       { \pos _ -> LBRACK pos }
}                       { \pos _ -> RBRACK pos }
;                       { \pos _ -> SEMI pos }
,                       { \pos _ -> COMMA pos }
(                       { \pos _ -> LPARA pos }
)                       { \pos _ -> RPARA pos }
=                       { \pos _ -> ASSIGN pos }
if                      { \pos _ -> IF pos }
else                    { \pos _ -> ELSE pos }
while                   { \pos _ -> WHILE pos }
for                     { \pos _ -> FOR pos }
return                  { \pos _ -> RETURN pos }
True|False|Maybe        { \pos s -> BOOL pos (head s) }
$alpha($alpha|$digit)*  { \pos s -> ID pos s }

{
data Token = COMP AlexPosn [Char]
           | ARIT AlexPosn [Char]
           | NUM AlexPosn Int
           | REAL AlexPosn Double
           | LBRACK AlexPosn
           | RBRACK AlexPosn
           | SEMI AlexPosn
           | COMMA AlexPosn
           | LPARA AlexPosn
           | RPARA AlexPosn
           | ASSIGN AlexPosn
           | IF AlexPosn
           | ELSE AlexPosn
           | WHILE AlexPosn
           | FOR AlexPosn
           | RETURN AlexPosn
           | BOOL AlexPosn Char
           | ID AlexPosn [Char]
    deriving (Eq,Show)
}
