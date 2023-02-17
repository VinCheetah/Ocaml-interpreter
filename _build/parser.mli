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
  | EOF
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
  | REC
  | TRUE
  | FALSE
  | AND
  | OR
  | NOT
  | PRINT
  | FUN
  | FLECHE
  | SCOLON
  | DSCOLON
  | UNIT
  | REF
  | EXCL
  | REVAL
  | UNDERSCORE
  | E
  | RAISE
  | TRY
  | WITH
  | INCR

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.expr
