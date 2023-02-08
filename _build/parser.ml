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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

# 39 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\001\000\002\000\004\000\
\001\000\006\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\005\000\000\000\000\000\000\000\000\000\
\003\000\004\000\000\000\000\000\028\000\000\000\022\000\025\000\
\012\000\000\000\000\000\000\000\000\000\023\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\008\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000"

let yysindex = "\004\000\
\035\255\000\000\000\000\000\000\035\255\035\255\035\255\014\255\
\000\000\000\000\035\255\006\255\000\000\106\255\000\000\000\000\
\000\000\129\255\144\255\001\255\228\255\000\000\035\255\035\255\
\035\255\035\255\035\255\035\255\000\000\035\255\035\255\035\255\
\035\255\035\255\035\255\035\255\035\255\000\000\035\255\035\255\
\167\255\038\255\000\000\038\255\000\000\005\255\160\255\160\255\
\160\255\160\255\160\255\160\255\243\255\228\255\182\255\205\255\
\000\000\035\255\035\255\228\255\228\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\250\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\060\255\000\000\083\255\000\000\003\000\016\000\029\000\
\042\000\055\000\068\000\081\000\094\000\029\255\000\000\000\000\
\000\000\000\000\000\000\105\255\204\255"

let yygindex = "\000\000\
\000\000\251\255\000\000\000\000"

let yytablesize = 375
let yytable = "\017\000\
\018\000\019\000\021\000\021\000\001\000\021\000\022\000\024\000\
\025\000\026\000\027\000\021\000\021\000\023\000\021\000\020\000\
\040\000\041\000\042\000\043\000\044\000\045\000\046\000\000\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\000\000\055\000\056\000\003\000\004\000\019\000\019\000\005\000\
\000\000\025\000\006\000\027\000\000\000\000\000\019\000\019\000\
\000\000\019\000\000\000\007\000\060\000\061\000\008\000\000\000\
\009\000\010\000\000\000\000\000\011\000\012\000\007\000\000\000\
\007\000\000\000\007\000\000\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\000\000\007\000\007\000\000\000\
\007\000\000\000\000\000\007\000\007\000\009\000\000\000\009\000\
\000\000\009\000\000\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\000\000\009\000\009\000\000\000\009\000\
\000\000\000\000\009\000\009\000\024\000\025\000\026\000\027\000\
\028\000\026\000\026\000\029\000\030\000\031\000\032\000\033\000\
\034\000\035\000\026\000\026\000\000\000\026\000\000\000\000\000\
\000\000\036\000\037\000\024\000\025\000\026\000\027\000\028\000\
\000\000\038\000\000\000\030\000\031\000\032\000\033\000\034\000\
\035\000\000\000\024\000\025\000\026\000\027\000\028\000\000\000\
\036\000\037\000\030\000\031\000\032\000\033\000\034\000\035\000\
\000\000\039\000\024\000\025\000\026\000\027\000\028\000\036\000\
\037\000\024\000\025\000\026\000\027\000\028\000\000\000\057\000\
\000\000\030\000\031\000\032\000\033\000\034\000\035\000\000\000\
\024\000\025\000\026\000\027\000\028\000\000\000\036\000\037\000\
\030\000\031\000\032\000\033\000\034\000\035\000\000\000\000\000\
\058\000\000\000\000\000\000\000\000\000\036\000\037\000\024\000\
\025\000\026\000\027\000\028\000\027\000\027\000\000\000\030\000\
\031\000\032\000\033\000\034\000\035\000\027\000\027\000\000\000\
\027\000\059\000\000\000\000\000\036\000\037\000\024\000\025\000\
\026\000\027\000\028\000\000\000\000\000\000\000\030\000\031\000\
\032\000\033\000\034\000\035\000\000\000\024\000\025\000\026\000\
\027\000\028\000\000\000\036\000\037\000\030\000\031\000\032\000\
\033\000\034\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\000\000\011\000\011\000\000\000\011\000\
\013\000\013\000\011\000\011\000\000\000\000\000\000\000\000\000\
\000\000\013\000\013\000\000\000\013\000\014\000\014\000\013\000\
\013\000\000\000\000\000\000\000\000\000\000\000\014\000\014\000\
\000\000\014\000\015\000\015\000\014\000\014\000\000\000\000\000\
\000\000\000\000\000\000\015\000\015\000\000\000\015\000\016\000\
\016\000\015\000\015\000\000\000\000\000\000\000\000\000\000\000\
\016\000\016\000\000\000\016\000\018\000\018\000\016\000\016\000\
\000\000\000\000\000\000\000\000\000\000\018\000\018\000\000\000\
\018\000\017\000\017\000\018\000\018\000\000\000\000\000\000\000\
\000\000\000\000\017\000\017\000\000\000\017\000\020\000\020\000\
\017\000\017\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\000\000\020\000\000\000\000\000\000\000\020\000"

let yycheck = "\005\000\
\006\000\007\000\009\001\010\001\001\000\011\000\001\001\003\001\
\004\001\005\001\006\001\018\001\019\001\008\001\021\001\002\001\
\016\001\023\000\024\000\025\000\026\000\027\000\028\000\255\255\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\255\255\039\000\040\000\001\001\002\001\009\001\010\001\005\001\
\255\255\004\001\008\001\006\001\255\255\255\255\018\001\019\001\
\255\255\021\001\255\255\017\001\058\000\059\000\020\001\255\255\
\022\001\023\001\255\255\255\255\026\001\027\001\003\001\255\255\
\005\001\255\255\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\255\255\
\021\001\255\255\255\255\024\001\025\001\003\001\255\255\005\001\
\255\255\007\001\255\255\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\255\255\021\001\
\255\255\255\255\024\001\025\001\003\001\004\001\005\001\006\001\
\007\001\009\001\010\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\018\001\019\001\255\255\021\001\255\255\255\255\
\255\255\024\001\025\001\003\001\004\001\005\001\006\001\007\001\
\255\255\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\255\255\003\001\004\001\005\001\006\001\007\001\255\255\
\024\001\025\001\011\001\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\003\001\004\001\005\001\006\001\007\001\024\001\
\025\001\003\001\004\001\005\001\006\001\007\001\255\255\009\001\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\255\255\
\003\001\004\001\005\001\006\001\007\001\255\255\024\001\025\001\
\011\001\012\001\013\001\014\001\015\001\016\001\255\255\255\255\
\019\001\255\255\255\255\255\255\255\255\024\001\025\001\003\001\
\004\001\005\001\006\001\007\001\009\001\010\001\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\018\001\019\001\255\255\
\021\001\021\001\255\255\255\255\024\001\025\001\003\001\004\001\
\005\001\006\001\007\001\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\255\255\003\001\004\001\005\001\
\006\001\007\001\255\255\024\001\025\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\009\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\255\255\021\001\
\009\001\010\001\024\001\025\001\255\255\255\255\255\255\255\255\
\255\255\018\001\019\001\255\255\021\001\009\001\010\001\024\001\
\025\001\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\255\255\021\001\009\001\010\001\024\001\025\001\255\255\255\255\
\255\255\255\255\255\255\018\001\019\001\255\255\021\001\009\001\
\010\001\024\001\025\001\255\255\255\255\255\255\255\255\255\255\
\018\001\019\001\255\255\021\001\009\001\010\001\024\001\025\001\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\255\255\
\021\001\009\001\010\001\024\001\025\001\255\255\255\255\255\255\
\255\255\255\255\018\001\019\001\255\255\021\001\009\001\010\001\
\024\001\025\001\255\255\255\255\255\255\255\255\255\255\018\001\
\019\001\255\255\021\001\255\255\255\255\255\255\025\001"

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
# 44 "parser.mly"
                              ( _1 )
# 260 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 49 "parser.mly"
                                        ( Const _1 )
# 267 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                                        ( BConst true)
# 273 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
                                        ( BConst false )
# 279 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                                        ( Var _1 )
# 286 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 53 "parser.mly"
                                        ( _2 )
# 293 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 54 "parser.mly"
                                        ( ArithOp (Add,_1,_3) )
# 301 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 55 "parser.mly"
                                        ( ArithOp (Mul,_1,_3) )
# 309 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 56 "parser.mly"
                                        ( ArithOp (Min,_1,_3) )
# 317 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 57 "parser.mly"
                                        ( ArithOp (Div,_1,_3) )
# 325 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 58 "parser.mly"
                                        ( ArithOp (Mod,_1,_3) )
# 333 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 59 "parser.mly"
                                        ( ArithOp (Min,Const 0, _2) )
# 340 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 60 "parser.mly"
                                        ( CompOp (L,_1,_3) )
# 348 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 61 "parser.mly"
                                        ( CompOp (Le,_1,_3) )
# 356 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 62 "parser.mly"
                                        ( CompOp (G,_1,_3) )
# 364 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 63 "parser.mly"
                                        ( CompOp (Ge,_1,_3) )
# 372 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 64 "parser.mly"
                                        ( CompOp (Eq,_1,_3) )
# 380 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 65 "parser.mly"
                                        ( CompOp (Ne,_1,_3) )
# 388 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 66 "parser.mly"
                                        ( BoolOp (Or,_1,_3) )
# 396 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 67 "parser.mly"
                                        ( BoolOp (And,_1,_3) )
# 404 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 68 "parser.mly"
                                        ( BoolOp (Not,_2, BConst true) )
# 411 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 69 "parser.mly"
                                        ( _1 )
# 418 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
                                        ( PrInt (Const _2) )
# 425 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 71 "parser.mly"
                                        ( PrInt _3 )
# 432 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 72 "parser.mly"
                                        ( _1 )
# 439 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 76 "parser.mly"
                                                 ( If(_2,_4,_6) )
# 448 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 80 "parser.mly"
                                        ( Let(_2,_4,_6) )
# 457 "parser.ml"
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
