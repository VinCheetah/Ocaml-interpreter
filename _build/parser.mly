%{
(* --- préambule: ici du code Caml --- *)

open Expr   (* rappel: dans expr.ml: 
             type expr = Const of int | Add of expr*expr | Mull of expr*expr *)

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <string> VAR 
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token EOL             /* retour à la ligne */
%token L LE G GE NE 
%token IF THEN ELSE
%token OP
%token PRINT
%token LET 
%token IN 
%token EQ 
%token AND OR 

%right LET IN  
%left AND OR 
%left L 
%left LE
%left G GE NE
%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES  /* associativité gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert à "marquer" une règle pour lui donner la précédence maximale */
%nonassoc IF THEN ELSE 
%right PRINT
%start main             /* "start" signale le point d'entrée: */
                        /* c'est ici main, qui est défini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associé au point d'entrée */

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
expression EOL                { $1 }  /* on veut reconnaître une expression */
  ;
  

expression:			    /* règles de grammaire pour les expressions */
  | VAR                                 { Var $1 }
  | INT                                 { Const $1 }
  | LPAREN expression RPAREN            { $2 } /* on récupère le deuxième élément */
  | expression PLUS expression          { Add($1,$3) }
  | expression TIMES expression         { Mul($1,$3) }
  | expression MINUS expression         { Min($1,$3) }
  /* | expression L expression            { L($1,$3 ) }*/
  | MINUS expression %prec UMINUS       { Min(Const 0, $2) }
  | expression L expression            { Op(L,$1,$3)} /* je voulais mettre expression operator expression pour ne pas écrire toutes les règles mais ça me provoquait des conflits sur un seul état que je ne comprenais pas */ 
  | expression LE expression            { Op(Le,$1,$3)}
  | expression G expression            { Op(G,$1,$3)}
  | expression GE expression            { Op(Ge,$1,$3)}
  | expression NE expression            { Op(Ne,$1,$3)}
  
  |condition                            { $1 }
  | PRINT expression                    { PrInt $2}
  | declaration                         { $1 }

/*operator:  essai de 
  | L                                     { L } 
  | LE                                    {LE} */  
condition:
  |IF expression THEN expression ELSE expression { If($2,$4,$6) }


declaration : 
  | LET VAR EQ expression IN expression { Let($2,$4,$6) }


