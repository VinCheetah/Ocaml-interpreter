type token =
  | INT of (int)
  | VAR of (string)
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | MOD
  | LPAREN
  | RPAREN
  | EOL
  | L
  | LE
  | G
  | GE
  | NE
  | EQ
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | PRINT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
