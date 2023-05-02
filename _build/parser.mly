%{
(* --- préambule: ici du code Caml --- *)

open Types   (* rappel: dans Types.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT 
%token <string> VAR 
%token PLUS TIMES MINUS DIV MOD
%token LPAREN RPAREN
%token BEGIN END
%token EOF  
%token L LE G GE NE EQ
%token IF THEN ELSE
%token LET IN REC
%token TRUE FALSE
%token AND OR NOT
%token PRINT
%token FUN FLECHE FUNCTION
%token SCOLON DSCOLON
%token UNIT REF EXCL REVAL
%token UNDERSCORE
%token E RAISE
%token TRY MATCH WITH PIPE
%token INCR DECR
%token COMMA
%token LLIST RLIST EMPTYLIST CONS




%right DSCOLON
%nonassoc SCOLONLIST
%nonassoc UNDER
%right LET IN
%nonassoc IF THEN ELSE
%left FUN FLECHE
%left COMMA
%nonassoc LLIST RLIST
%right CONS
%nonassoc UNIT
%right REVAL
%right OR  
%right AND
%nonassoc L LE G GE NE EQ
%nonassoc NOT
%nonassoc MOD
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV /* associativité gauche: a*b*c, c'est (a*b)*c */
%right REF
%nonassoc E RAISE TRY WITH 
%nonassoc INCR DECR
%nonassoc PRINT
%nonassoc VAR
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%right EXCL
%nonassoc REC PIPE
%left SCOLON
%nonassoc LPAREN RPAREN BEGIN END


%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Types.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
top_expr EOF                                           { $1 }  /* on veut reconnaître une expression */
  ;
  
top_expr : expr_seq                                    { $1 }


expr_seq: /* règles de grammaire pour les expressions afin de différencier les ; des listes des ; des séquences */
  | expression %prec UNDER                             { $1 }
  | expr_seq SCOLON expr_seq                           { Seq ($1,$3) }



expression:			    /* règles de grammaire pour les expressions */
  | INT                                                { Const $1 }
  | TRUE                                               { BConst true }
  | FALSE                                              { BConst false }
  | VAR                                                { Var (MNom $1) }
  | UNIT                                               { Unit }
  | LPAREN expr_seq RPAREN                             { $2 }
  | BEGIN expr_seq END                                 { $2 }
  | expression PLUS expression                         { ArithOp (Add,$1,$3) }
  | expression TIMES expression                        { ArithOp (Mul,$1,$3) }
  | expression MINUS expression                        { ArithOp (Min,$1,$3) }
  | expression DIV expression                          { ArithOp (Div,$1,$3) }
  | expression MOD expression                          { ArithOp (Mod,$1,$3) }
  | MINUS expression %prec UMINUS                      { ArithOp (Min,Const 0, $2) }
  | expression L expression                            { CompOp (L,$1,$3) } 
  | expression LE expression                           { CompOp (Le,$1,$3) }
  | expression G expression                            { CompOp (G,$1,$3) }
  | expression GE expression                           { CompOp (Ge,$1,$3) }
  | expression EQ expression                           { CompOp (Eq,$1,$3) }
  | expression NE expression                           { CompOp (Ne,$1,$3) }
  | expression OR expression                           { BoolOp (Or,$1,$3) }
  | expression AND expression                          { BoolOp (And,$1,$3) }
  | NOT expression                                     { BoolOp (Not,$2, BConst true) }    /* On ajoute un troisième élément pour qu'elle s'intègre dans le type BoolOp */
  | IF expression THEN expression ELSE expression      { If ($2,$4,$6) }
  | LET motif corps_func groupe_decla expr_seq         { Let (false,$2,$3,$4,$5) }
  | LET REC motif corps_func groupe_decla expr_seq     { Let (true,$3,$4,$5,$6) }
  | expression REVAL expression                        { RefNew ($1,$3) }
  | expression COMMA expression                        { CoupleExpr ($1,$3) }
  | E expression                                       { Exn $2 }
  | RAISE expression                                   { Raise $2 }
  | TRY expr_seq WITH pattern                          { TryWith ($2,$4) }
  | EXCL sexpr                                         { ValRef ($2) }
  | REF sexpr                                          { Ref $2 }   
  | INCR sexpr                                         { InDecr ($2,true) }
  | DECR sexpr                                         { InDecr ($2,false) }
  | PRINT sexpr                                        { PrInt ($2) }
  | FUN motif corps_func                               { Fun ($2,$3) }                        
  | applic                                             { $1 }
  | expression CONS expression                         { Cons ($1,$3) }
  | EMPTYLIST                                          { EmptyList }
  | LLIST liste RLIST                                  { $2 }          
  | MATCH expression WITH pattern                      { MatchWith ($2,$4) }             


motif :
  | LPAREN motif RPAREN                                { $2 }
  | motif COMMA motif                                  { MCouple ($1,$3) }
  | VAR                                                { MNom $1 }
  | UNDERSCORE                                         { MNone }
  | UNIT                                               { MUnit }
  | motif CONS motif                                   { MCons ($1,$3) }
  | EMPTYLIST                                          { MEmptyList }
  | E motif                                            { MExcp ($2) }
  | INT                                                { MExpr (Const $1)}
  | TRUE                                               { MExpr (BConst true)}                                              
  | FALSE                                              { MExpr (BConst false)}     



sexpr: /* règles de grammaire pour les suites d'expressions */
  | LPAREN expr_seq RPAREN                             { $2 } 
  | VAR                                                { Var (MNom $1) }
  | INT                                                { Const $1 }
  | TRUE                                               { BConst true}
  | FALSE                                              { BConst false }
  | EXCL sexpr                                         { ValRef ($2) }
  | UNIT                                               { Unit }
  | LLIST liste RLIST                                  { $2 }
  | EMPTYLIST                                          { EmptyList }


corps_func :
  | motif corps_func                                   { Fun ($1,$2) }
  | EQ expr_seq                                        { $2 }
  | FLECHE expr_seq                                    { $2 }
  | EQ FUNCTION pattern                                { Fun (MNom "_",MatchWith(Var (MNom "_"),$3)) } /* On utilise un caractère interdit pour ne jamais avoir de conflits*/
  | FLECHE FUNCTION pattern                            { Fun (MNom "_",MatchWith(Var (MNom "_"),$3)) } 


applic: /* règles de grammaire pour les applications */
  | applic sexpr                                       { App ($1,$2) }
  | sexpr sexpr                                        { App ($1,$2) }


groupe_decla:
  | IN                                                 { false }
  | DSCOLON                                            { true }


liste: /* règles de grammaire pour les listes */  
  | expression SCOLON liste                            { Cons ($1,$3) }
  | expression                                         { Cons ($1,EmptyList) }


pattern: /* règles de grammaire pour les pattern matching */
  | PIPE pattern                                       { $2 }
  | motif FLECHE expr_seq PIPE pattern                 { ($1,$3)::$5 }
  | motif FLECHE expr_seq                              { [($1,$3)] }