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
%token EOF  
%token L LE G GE NE EQ
%token IF THEN ELSE
%token LET IN REC
%token TRUE FALSE
%token AND OR NOT
%token PRINT
%token FUN FLECHE
%token SCOLON
%token UNIT REF EXCL REVAL
%token UNDERSCORE
%token E RAISE
%token TRY WITH



%nonassoc IF THEN ELSE 
%left FUN FLECHE
%left LET IN
%left SCOLON
%nonassoc REF UNIT REVAL
%right OR  
%right AND
%nonassoc L LE G GE NE EQ
%nonassoc NOT
%nonassoc MOD
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV /* associativité gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%nonassoc EXCL
%nonassoc PRINT
%nonassoc LPAREN RPAREN

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
  | INT                                                       { Const $1 }
  | TRUE                                                      { BConst true}
  | FALSE                                                     { BConst false }
  | VAR                                                       { Var (Nom $1) }
  | UNIT                                                      { Unit }
  | applic                                                    { $1 }
  | LPAREN expression RPAREN                                  { $2 }
  | expression PLUS expression                                { ArithOp (Add,$1,$3) }
  | expression TIMES expression                               { ArithOp (Mul,$1,$3) }
  | expression MINUS expression                               { ArithOp (Min,$1,$3) }
  | expression DIV expression                                 { ArithOp (Div,$1,$3) }
  | expression MOD expression                                 { ArithOp (Mod,$1,$3) }
  | MINUS expression %prec UMINUS                             { ArithOp (Min,Const 0, $2) }
  | expression L expression                                   { CompOp (L,$1,$3) } 
  | expression LE expression                                  { CompOp (Le,$1,$3) }
  | expression G expression                                   { CompOp (G,$1,$3) }
  | expression GE expression                                  { CompOp (Ge,$1,$3) }
  | expression EQ expression                                  { CompOp (Eq,$1,$3) }
  | expression NE expression                                  { CompOp (Ne,$1,$3) }
  | expression OR expression                                  { BoolOp (Or,$1,$3) }
  | expression AND expression                                 { BoolOp (And,$1,$3) }
  | NOT expression                                            { BoolOp (Not,$2, BConst true) }    /* On ajoute un troisième élément pour qu'elle s'intègre dans le type BoolOp */
  | condition                                                 { $1 }
  | PRINT INT                                                 { PrInt (Const $2) }
  | PRINT LPAREN expression RPAREN                            { PrInt $3 }
  | declaration                                               { $1 }
  | FUN VAR FLECHE expression                                 { Fun (Nom $2,$4) }
  | expression SCOLON expression                              { Seq ($1,$3) }
  | REF expression                                            { Ref $2 }   
  | EXCL expression                                           { ValRef $2 }
  | expression REVAL expression                               { RefNew ($1,$3) }
  | RAISE LPAREN E expression RPAREN                          { Raise $4 }
  | TRY expression WITH LPAREN E expression RPAREN FLECHE expression { TryWith($2,$6,$9) }

sexpr:
  | INT                                                       { Const $1 }
  | TRUE                                                      { BConst true}
  | FALSE                                                     { BConst false }
  | VAR                                                       { Var (Nom $1) }
  | UNIT                                                      { Unit }
  | LPAREN expression RPAREN                                  { $2 }

applic:
  | applic sexpr                                              { App ($1,$2) }
  | VAR sexpr                                                 { App (Var (Nom $1),$2) }
  | LPAREN FUN VAR FLECHE expression RPAREN sexpr             { App (Fun(Nom $3,$5),$7) }

condition:
  | IF expression THEN expression ELSE expression             { If($2,$4,$6) }

declaration : 
  | LET REC VAR EQ expression IN expression                   { Let (Nom $3,true,$5,$7) }
  | LET VAR EQ expression IN expression                       { Let (Nom $2,false,$4,$6) }
  | LET REC UNDERSCORE EQ expression IN expression            { Let (None,true,$5,$7) }
  | LET UNDERSCORE EQ expression IN expression                { Let (None,false,$4,$6) }


