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
  | FUN
  | FLECHE

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

# 41 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* TIMES *);
  261 (* MINUS *);
  262 (* DIV *);
  263 (* MOD *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* EOL *);
  267 (* L *);
  268 (* LE *);
  269 (* G *);
  270 (* GE *);
  271 (* NE *);
  272 (* EQ *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* LET *);
  277 (* IN *);
  278 (* TRUE *);
  279 (* FALSE *);
  280 (* AND *);
  281 (* OR *);
  282 (* NOT *);
  283 (* PRINT *);
  284 (* FUN *);
  285 (* FLECHE *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\001\000\002\000\004\000\
\001\000\004\000\002\000\006\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\005\000\000\000\000\000\000\000\000\000\
\003\000\004\000\000\000\000\000\000\000\030\000\000\000\022\000\
\025\000\000\000\000\000\000\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\000\000"

let yydgoto = "\002\000\
\014\000\040\000\016\000\017\000"

let yysindex = "\008\000\
\040\000\000\000\000\000\000\000\040\000\040\000\040\000\255\254\
\000\000\000\000\040\000\012\255\016\255\000\000\044\255\000\000\
\000\000\173\000\072\255\100\255\005\255\012\000\000\000\040\000\
\251\254\040\000\040\000\040\000\040\000\040\000\000\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\212\255\
\000\000\040\000\040\000\128\255\040\000\181\000\173\000\173\000\
\173\000\157\000\149\000\149\000\149\000\149\000\149\000\149\000\
\122\000\095\000\156\255\184\255\000\000\212\255\040\000\040\000\
\240\255\068\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\206\000\000\000\000\000\000\000\254\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\249\254\
\000\000\000\000\000\000\000\000\000\000\052\001\232\000\002\001\
\028\001\072\001\085\001\098\001\111\001\124\001\137\001\150\001\
\163\001\176\001\000\000\000\000\000\000\034\000\000\000\000\000\
\202\001\189\001"

let yygindex = "\000\000\
\000\000\255\255\000\000\000\000"

let yytablesize = 735
let yytable = "\015\000\
\021\000\027\000\027\000\018\000\019\000\020\000\021\000\021\000\
\001\000\022\000\027\000\027\000\023\000\027\000\021\000\021\000\
\021\000\025\000\021\000\024\000\043\000\000\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\000\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\000\000\000\000\
\059\000\060\000\000\000\062\000\003\000\004\000\026\000\027\000\
\028\000\029\000\030\000\006\000\000\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\007\000\065\000\066\000\008\000\
\000\000\009\000\010\000\038\000\039\000\011\000\012\000\013\000\
\003\000\004\000\026\000\027\000\028\000\029\000\030\000\006\000\
\041\000\000\000\032\000\033\000\034\000\035\000\036\000\037\000\
\007\000\000\000\000\000\008\000\000\000\009\000\010\000\038\000\
\039\000\011\000\012\000\013\000\003\000\004\000\026\000\027\000\
\028\000\029\000\030\000\006\000\000\000\000\000\032\000\033\000\
\034\000\035\000\036\000\037\000\007\000\042\000\000\000\008\000\
\000\000\009\000\010\000\038\000\039\000\011\000\012\000\013\000\
\003\000\004\000\026\000\027\000\028\000\029\000\030\000\006\000\
\061\000\000\000\032\000\033\000\034\000\035\000\036\000\037\000\
\007\000\000\000\000\000\008\000\000\000\009\000\010\000\038\000\
\039\000\011\000\012\000\013\000\003\000\004\000\026\000\027\000\
\028\000\029\000\030\000\006\000\000\000\000\000\032\000\033\000\
\034\000\035\000\036\000\037\000\007\000\000\000\063\000\008\000\
\000\000\009\000\010\000\038\000\039\000\011\000\012\000\013\000\
\003\000\004\000\026\000\027\000\028\000\029\000\030\000\006\000\
\000\000\000\000\032\000\033\000\034\000\035\000\036\000\037\000\
\007\000\000\000\000\000\008\000\064\000\009\000\010\000\038\000\
\039\000\011\000\012\000\013\000\003\000\004\000\026\000\027\000\
\028\000\029\000\030\000\006\000\000\000\000\000\032\000\033\000\
\034\000\035\000\036\000\037\000\007\000\000\000\000\000\008\000\
\000\000\009\000\010\000\038\000\039\000\011\000\012\000\013\000\
\003\000\004\000\026\000\027\000\028\000\029\000\030\000\006\000\
\000\000\000\000\032\000\033\000\034\000\035\000\036\000\037\000\
\000\000\000\000\000\000\008\000\000\000\009\000\010\000\038\000\
\039\000\011\000\012\000\013\000\003\000\004\000\026\000\027\000\
\028\000\029\000\030\000\006\000\000\000\000\000\032\000\033\000\
\034\000\035\000\036\000\037\000\000\000\000\000\000\000\008\000\
\000\000\009\000\010\000\038\000\039\000\000\000\012\000\013\000\
\003\000\004\000\026\000\026\000\005\000\000\000\000\000\006\000\
\000\000\000\000\000\000\026\000\026\000\000\000\026\000\000\000\
\007\000\000\000\000\000\008\000\000\000\009\000\010\000\000\000\
\000\000\011\000\012\000\013\000\003\000\004\000\026\000\027\000\
\028\000\029\000\030\000\006\000\000\000\000\000\032\000\033\000\
\034\000\035\000\036\000\037\000\000\000\000\000\000\000\008\000\
\000\000\009\000\010\000\038\000\039\000\000\000\012\000\003\000\
\004\000\026\000\027\000\028\000\029\000\030\000\006\000\000\000\
\000\000\032\000\033\000\034\000\035\000\036\000\037\000\000\000\
\000\000\000\000\000\000\000\000\009\000\010\000\038\000\039\000\
\000\000\012\000\003\000\004\000\026\000\027\000\028\000\029\000\
\030\000\006\000\000\000\000\000\032\000\033\000\034\000\035\000\
\036\000\037\000\000\000\000\000\000\000\000\000\000\000\009\000\
\010\000\038\000\000\000\000\000\012\000\003\000\004\000\026\000\
\027\000\028\000\029\000\030\000\006\000\003\000\004\000\026\000\
\027\000\028\000\029\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\009\000\010\000\000\000\003\000\004\000\012\000\
\000\000\000\000\009\000\010\000\006\000\003\000\004\000\012\000\
\027\000\000\000\029\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\009\000\010\000\000\000\000\000\000\000\012\000\
\000\000\000\000\009\000\010\000\000\000\000\000\000\000\012\000\
\012\000\012\000\012\000\012\000\012\000\000\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\000\000\000\000\012\000\012\000\012\000\
\000\000\012\000\008\000\008\000\008\000\008\000\008\000\000\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\000\000\008\000\
\008\000\008\000\000\000\008\000\009\000\012\000\009\000\012\000\
\009\000\000\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\000\000\
\000\000\009\000\009\000\009\000\000\000\009\000\010\000\010\000\
\010\000\010\000\010\000\000\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\000\000\000\000\010\000\010\000\010\000\007\000\010\000\
\007\000\000\000\007\000\000\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\000\000\000\000\007\000\007\000\007\000\000\000\007\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\013\000\013\000\011\000\
\011\000\011\000\000\000\011\000\000\000\013\000\013\000\013\000\
\013\000\013\000\014\000\014\000\013\000\013\000\013\000\000\000\
\013\000\000\000\014\000\014\000\014\000\014\000\014\000\015\000\
\015\000\014\000\014\000\014\000\000\000\014\000\000\000\015\000\
\015\000\015\000\015\000\015\000\016\000\016\000\015\000\015\000\
\015\000\000\000\015\000\000\000\016\000\016\000\016\000\016\000\
\016\000\018\000\018\000\016\000\016\000\016\000\000\000\016\000\
\000\000\018\000\018\000\018\000\018\000\018\000\017\000\017\000\
\018\000\018\000\018\000\000\000\018\000\000\000\017\000\017\000\
\017\000\017\000\017\000\020\000\020\000\017\000\017\000\017\000\
\000\000\017\000\000\000\020\000\020\000\020\000\020\000\020\000\
\019\000\019\000\000\000\020\000\020\000\000\000\020\000\000\000\
\019\000\019\000\019\000\019\000\019\000\029\000\029\000\000\000\
\000\000\019\000\000\000\019\000\000\000\029\000\029\000\029\000\
\000\000\029\000\028\000\028\000\000\000\000\000\029\000\000\000\
\029\000\000\000\000\000\028\000\028\000\000\000\028\000"

let yycheck = "\001\000\
\002\001\009\001\010\001\005\000\006\000\007\000\009\001\010\001\
\001\000\011\000\018\001\019\001\001\001\021\001\017\001\018\001\
\019\001\002\001\021\001\008\001\016\001\255\255\024\000\029\001\
\026\000\027\000\028\000\029\000\030\000\255\255\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\255\255\255\255\
\042\000\043\000\255\255\045\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\063\000\064\000\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\020\001\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\020\001\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\255\255\019\001\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\255\255\255\255\020\001\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\255\255\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\255\255\255\255\020\001\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\255\255\255\255\020\001\
\255\255\022\001\023\001\024\001\025\001\255\255\027\001\028\001\
\001\001\002\001\009\001\010\001\005\001\255\255\255\255\008\001\
\255\255\255\255\255\255\018\001\019\001\255\255\021\001\255\255\
\017\001\255\255\255\255\020\001\255\255\022\001\023\001\255\255\
\255\255\026\001\027\001\028\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\255\255\255\255\020\001\
\255\255\022\001\023\001\024\001\025\001\255\255\027\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\255\255\027\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\024\001\255\255\255\255\027\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\255\255\001\001\002\001\027\001\
\255\255\255\255\022\001\023\001\008\001\001\001\002\001\027\001\
\004\001\255\255\006\001\255\255\008\001\255\255\255\255\255\255\
\255\255\255\255\022\001\023\001\255\255\255\255\255\255\027\001\
\255\255\255\255\022\001\023\001\255\255\255\255\255\255\027\001\
\003\001\004\001\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\255\255\255\255\024\001\025\001\026\001\
\255\255\028\001\003\001\004\001\005\001\006\001\007\001\255\255\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\255\255\255\255\024\001\
\025\001\026\001\255\255\028\001\003\001\004\001\005\001\006\001\
\007\001\255\255\009\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\255\255\
\255\255\024\001\025\001\026\001\255\255\028\001\003\001\004\001\
\005\001\006\001\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\255\255\024\001\025\001\026\001\003\001\028\001\
\005\001\255\255\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\255\255\255\255\024\001\025\001\026\001\255\255\028\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\009\001\010\001\024\001\
\025\001\026\001\255\255\028\001\255\255\017\001\018\001\019\001\
\020\001\021\001\009\001\010\001\024\001\025\001\026\001\255\255\
\028\001\255\255\017\001\018\001\019\001\020\001\021\001\009\001\
\010\001\024\001\025\001\026\001\255\255\028\001\255\255\017\001\
\018\001\019\001\020\001\021\001\009\001\010\001\024\001\025\001\
\026\001\255\255\028\001\255\255\017\001\018\001\019\001\020\001\
\021\001\009\001\010\001\024\001\025\001\026\001\255\255\028\001\
\255\255\017\001\018\001\019\001\020\001\021\001\009\001\010\001\
\024\001\025\001\026\001\255\255\028\001\255\255\017\001\018\001\
\019\001\020\001\021\001\009\001\010\001\024\001\025\001\026\001\
\255\255\028\001\255\255\017\001\018\001\019\001\020\001\021\001\
\009\001\010\001\255\255\025\001\026\001\255\255\028\001\255\255\
\017\001\018\001\019\001\020\001\021\001\009\001\010\001\255\255\
\255\255\026\001\255\255\028\001\255\255\017\001\018\001\019\001\
\255\255\021\001\009\001\010\001\255\255\255\255\026\001\255\255\
\028\001\255\255\255\255\018\001\019\001\255\255\021\001"

let yynames_const = "\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  MOD\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  L\000\
  LE\000\
  G\000\
  GE\000\
  NE\000\
  EQ\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LET\000\
  IN\000\
  TRUE\000\
  FALSE\000\
  AND\000\
  OR\000\
  NOT\000\
  PRINT\000\
  FUN\000\
  FLECHE\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 47 "parser.mly"
                              ( _1 )
# 359 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 52 "parser.mly"
                                        ( Const _1 )
# 366 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                                        ( BConst true)
# 372 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                                        ( BConst false )
# 378 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                                        ( Var _1 )
# 385 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 56 "parser.mly"
                                        ( _2 )
# 392 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 57 "parser.mly"
                                        ( ArithOp (Add,_1,_3) )
# 400 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 58 "parser.mly"
                                        ( ArithOp (Mul,_1,_3) )
# 408 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 59 "parser.mly"
                                        ( ArithOp (Min,_1,_3) )
# 416 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 60 "parser.mly"
                                        ( ArithOp (Div,_1,_3) )
# 424 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 61 "parser.mly"
                                        ( ArithOp (Mod,_1,_3) )
# 432 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 62 "parser.mly"
                                        ( ArithOp (Min,Const 0, _2) )
# 439 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 63 "parser.mly"
                                        ( CompOp (L,_1,_3) )
# 447 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 64 "parser.mly"
                                        ( CompOp (Le,_1,_3) )
# 455 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 65 "parser.mly"
                                        ( CompOp (G,_1,_3) )
# 463 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 66 "parser.mly"
                                        ( CompOp (Ge,_1,_3) )
# 471 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 67 "parser.mly"
                                        ( CompOp (Eq,_1,_3) )
# 479 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 68 "parser.mly"
                                        ( CompOp (Ne,_1,_3) )
# 487 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 69 "parser.mly"
                                        ( BoolOp (Or,_1,_3) )
# 495 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 70 "parser.mly"
                                        ( BoolOp (And,_1,_3) )
# 503 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 71 "parser.mly"
                                        ( BoolOp (Not,_2, BConst true) )
# 510 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 72 "parser.mly"
                                        ( _1 )
# 517 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
                                        ( PrInt (Const _2) )
# 524 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 74 "parser.mly"
                                        ( PrInt _3 )
# 531 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 75 "parser.mly"
                                        ( _1 )
# 538 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 76 "parser.mly"
                                        ( Fun (_2,_4) )
# 546 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 77 "parser.mly"
                                        ( App (_1,_2) )
# 554 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 81 "parser.mly"
                                                 ( If(_2,_4,_6) )
# 563 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 85 "parser.mly"
                                        ( Let(_2,_4,_6) )
# 572 "parser.ml"
               : 'declaration))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.expr)
