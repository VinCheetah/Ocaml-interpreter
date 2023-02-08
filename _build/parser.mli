type token =
  | INT of (int)
  | VAR of (string)
  | PLUS
  | TIMES
  | MINUS
  | LPAREN
  | RPAREN
  | EOL
  | L
  | LE
  | G
  | GE
  | NE
  | IF
  | THEN
  | ELSE
  | OP
  | PRINT
  | LET
  | IN
  | EQ
  | AND
  | OR

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.expr
