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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

# 35 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* TIMES *);
  261 (* MINUS *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* EOL *);
  265 (* L *);
  266 (* LE *);
  267 (* G *);
  268 (* GE *);
  269 (* NE *);
  270 (* IF *);
  271 (* THEN *);
  272 (* ELSE *);
  273 (* OP *);
  274 (* PRINT *);
  275 (* LET *);
  276 (* IN *);
  277 (* EQ *);
  278 (* AND *);
  279 (* OR *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\003\000\001\000\002\000\001\000\
\006\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\003\000\002\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\014\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000\013\000"

let yysindex = "\007\000\
\025\255\000\000\000\000\000\000\025\255\025\255\025\255\025\255\
\021\255\000\000\198\255\000\000\000\000\254\254\209\255\185\255\
\254\254\007\255\025\255\025\255\025\255\000\000\025\255\025\255\
\025\255\025\255\025\255\000\000\025\255\025\255\101\255\254\254\
\101\255\231\255\029\255\220\255\220\255\220\255\140\255\098\255\
\025\255\025\255\254\254\220\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\255\000\000\000\000\
\056\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\112\255\070\255\
\126\255\150\255\253\254\147\255\153\255\164\255\000\000\000\000\
\000\000\000\000\084\255\167\255"

let yygindex = "\000\000\
\000\000\251\255\000\000\000\000"

let yytablesize = 244
let yytable = "\014\000\
\015\000\016\000\017\000\010\000\010\000\010\000\010\000\001\000\
\025\000\026\000\027\000\010\000\010\000\031\000\032\000\033\000\
\010\000\034\000\035\000\036\000\037\000\038\000\018\000\039\000\
\040\000\003\000\004\000\030\000\000\000\005\000\006\000\019\000\
\020\000\021\000\000\000\043\000\044\000\000\000\007\000\025\000\
\026\000\027\000\008\000\009\000\008\000\008\000\008\000\000\000\
\008\000\008\000\008\000\008\000\000\000\000\000\000\000\000\000\
\008\000\008\000\015\000\015\000\015\000\008\000\015\000\015\000\
\015\000\015\000\000\000\000\000\000\000\000\000\015\000\015\000\
\006\000\006\000\006\000\015\000\006\000\006\000\006\000\006\000\
\000\000\000\000\000\000\000\000\006\000\006\000\017\000\017\000\
\017\000\006\000\017\000\017\000\017\000\017\000\000\000\000\000\
\000\000\000\000\017\000\017\000\019\000\020\000\021\000\017\000\
\020\000\000\000\023\000\024\000\025\000\026\000\027\000\025\000\
\026\000\027\000\005\000\000\000\005\000\042\000\005\000\005\000\
\005\000\005\000\000\000\000\000\000\000\000\000\005\000\005\000\
\007\000\000\000\007\000\005\000\007\000\007\000\007\000\007\000\
\000\000\000\000\000\000\000\000\007\000\007\000\019\000\020\000\
\021\000\007\000\000\000\000\000\023\000\024\000\025\000\026\000\
\027\000\011\000\011\000\041\000\009\000\009\000\009\000\012\000\
\012\000\011\000\011\000\000\000\009\000\009\000\011\000\012\000\
\012\000\009\000\013\000\013\000\012\000\018\000\018\000\000\000\
\000\000\000\000\013\000\013\000\000\000\018\000\018\000\013\000\
\000\000\000\000\018\000\019\000\020\000\021\000\000\000\000\000\
\000\000\023\000\024\000\025\000\026\000\027\000\000\000\029\000\
\019\000\020\000\021\000\000\000\000\000\022\000\023\000\024\000\
\025\000\026\000\027\000\019\000\020\000\021\000\000\000\028\000\
\000\000\023\000\024\000\025\000\026\000\027\000\019\000\020\000\
\021\000\000\000\000\000\000\000\023\000\024\000\025\000\026\000\
\027\000\019\000\020\000\021\000\000\000\000\000\000\000\000\000\
\024\000\025\000\026\000\027\000"

let yycheck = "\005\000\
\006\000\007\000\008\000\007\001\008\001\009\001\010\001\001\000\
\011\001\012\001\013\001\015\001\016\001\019\000\020\000\021\000\
\020\001\023\000\024\000\025\000\026\000\027\000\002\001\029\000\
\030\000\001\001\002\001\021\001\255\255\005\001\006\001\003\001\
\004\001\005\001\255\255\041\000\042\000\255\255\014\001\011\001\
\012\001\013\001\018\001\019\001\003\001\004\001\005\001\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\015\001\016\001\003\001\004\001\005\001\020\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\015\001\016\001\
\003\001\004\001\005\001\020\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\003\001\004\001\
\005\001\020\001\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\255\255\015\001\016\001\003\001\004\001\005\001\020\001\
\004\001\255\255\009\001\010\001\011\001\012\001\013\001\011\001\
\012\001\013\001\003\001\255\255\005\001\020\001\007\001\008\001\
\009\001\010\001\255\255\255\255\255\255\255\255\015\001\016\001\
\003\001\255\255\005\001\020\001\007\001\008\001\009\001\010\001\
\255\255\255\255\255\255\255\255\015\001\016\001\003\001\004\001\
\005\001\020\001\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\007\001\008\001\016\001\007\001\008\001\009\001\007\001\
\008\001\015\001\016\001\255\255\015\001\016\001\020\001\015\001\
\016\001\020\001\007\001\008\001\020\001\007\001\008\001\255\255\
\255\255\255\255\015\001\016\001\255\255\015\001\016\001\020\001\
\255\255\255\255\020\001\003\001\004\001\005\001\255\255\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\003\001\004\001\005\001\255\255\255\255\008\001\009\001\010\001\
\011\001\012\001\013\001\003\001\004\001\005\001\255\255\007\001\
\255\255\009\001\010\001\011\001\012\001\013\001\003\001\004\001\
\005\001\255\255\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\003\001\004\001\005\001\255\255\255\255\255\255\255\255\
\010\001\011\001\012\001\013\001"

let yynames_const = "\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  L\000\
  LE\000\
  G\000\
  GE\000\
  NE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  OP\000\
  PRINT\000\
  LET\000\
  IN\000\
  EQ\000\
  AND\000\
  OR\000\
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
# 208 "parser.ml"
               : Expr.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 49 "parser.mly"
                                        ( Var _1 )
# 215 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "parser.mly"
                                        ( Const _1 )
# 222 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 51 "parser.mly"
                                        ( _2 )
# 229 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 52 "parser.mly"
                                        ( Add(_1,_3) )
# 237 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 53 "parser.mly"
                                        ( Mul(_1,_3) )
# 245 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 54 "parser.mly"
                                        ( Min(_1,_3) )
# 253 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 56 "parser.mly"
                                        ( Min(Const 0, _2) )
# 260 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 57 "parser.mly"
                                       ( Op(L,_1,_3))
# 268 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 58 "parser.mly"
                                        ( Op(Le,_1,_3))
# 276 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 59 "parser.mly"
                                       ( Op(G,_1,_3))
# 284 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 60 "parser.mly"
                                        ( Op(Ge,_1,_3))
# 292 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 61 "parser.mly"
                                        ( Op(Ne,_1,_3))
# 300 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 63 "parser.mly"
                                        ( _1 )
# 307 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 64 "parser.mly"
                                        ( PrInt _2)
# 314 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 65 "parser.mly"
                                        ( _1 )
# 321 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 71 "parser.mly"
                                                 ( If(_2,_4,_6) )
# 330 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 75 "parser.mly"
                                        ( Let(_2,_4,_6) )
# 339 "parser.ml"
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
