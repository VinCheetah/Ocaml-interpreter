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
%token FUN FLECHE
%token SCOLON DSCOLON
%token UNIT REF EXCL REVAL
%token UNDERSCORE
%token E RAISE
%token TRY WITH
%token INCR
%token COMMA



%right DSCOLON
%left SCOLON
%right LET IN
%nonassoc IF THEN ELSE
%left FUN
%left FLECHE
%nonassoc UNIT REVAL
%right OR  
%right AND
%nonassoc L LE G GE NE EQ
%nonassoc NOT
%nonassoc MOD
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV /* associativité gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%nonassoc REF
%nonassoc E RAISE TRY WITH 
%nonassoc INCR
%nonassoc PRINT
%nonassoc VAR INT
%left COMMA
%nonassoc EXCL
%nonassoc REC
%nonassoc LPAREN RPAREN BEGIN END

%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Types.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
expression EOF                          { $1 }  /* on veut reconnaître une expression */
  ;
  

expression:			    /* règles de grammaire pour les expressions */
  | INT                                                { Const $1 }
  | TRUE                                               { BConst true}
  | FALSE                                              { BConst false }
  | VAR                                                { Var (MNom $1) }
  | UNIT                                               { Unit }
  | LPAREN expression RPAREN                           { $2 }
  | BEGIN expression END                               { $2 }
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
  | LET variable corps_func                            { Let ($2,false,$3) }
  | LET REC variable corps_func                        { Let ($3,true,$4) }
  | LET REC variable EQ expression                     { Let ($3,true,$5) }
  | LET variable EQ expression                         { Let ($2,false,$4) }
  | expression IN expression                           { In ($1,$3) }
  | expression DSCOLON                                 { $1 }
  | expression DSCOLON expression                      { Gseq ($1,$3) }
  | expression SCOLON expression                       { Seq ($1,$3) }
  | expression REVAL expression                        { RefNew ($1,$3) }
  | expression COMMA expression                        { CoupleExpr ($1,$3) }
  | E expression                                       { Exn $2 }
  | RAISE expression                                   { Raise $2 }
  | TRY expression WITH expression FLECHE expression   { TryWith ($2,$4,$6) }
  | EXCL sexpr                                         { ValRef ($2) }
  | REF sexpr                                          { Ref $2 }   
  | INCR sexpr                                         { Incr $2 }
  | PRINT sexpr                                        { PrInt ($2) }
  | func                                               { $1 }
  | applic                                             { $1 }

variable :
  | LPAREN variable RPAREN                             { $2 }
  | variable COMMA variable                            { MCouple ($1,$3) }
  | VAR                                                { MNom $1 }
  | UNDERSCORE                                         { MNone }


sexpr:
  | LPAREN expression RPAREN                           { $2 }
  | VAR                                                { Var (MNom $1) }
  | INT                                                { Const $1 }
  | TRUE                                               { BConst true}
  | FALSE                                              { BConst false }
  | EXCL sexpr                                         { ValRef ($2) }
  | UNIT                                               { Unit }

func :
  | VAR corps_func                                     { $2 }
  | FUN corps_func                                     { $2 }


corps_func :
  | variable corps_func                                { Fun ($1,$2) }
  | variable EQ expression                             { Fun ($1,$3) }
  | variable FLECHE expression                         { Fun ($1,$3) }


applic:
  | applic sexpr                                       { App ($1,$2) }
  | sexpr sexpr                                        { App ($1,$2) }



