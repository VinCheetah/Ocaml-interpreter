type comp_op =
  | Le
  | Ge
  | L
  | G
  | Eq
  | Ne


(* définition du type pour les opérateurs arithmétiques *)
type arith_op =
  | Add
  | Min
  | Mul
  | Div
  | Mod


(* défintion du type pour les opérateurs booléeens *)
type bool_op = 
  | And
  | Or
  | Not


 (* définition du type pour les expressions*) 
type expr =
  | Const   of int
  | BConst  of bool
  | Var     of string 
  | Unit
  | ArithOp of arith_op*expr*expr
  | CompOp  of comp_op*expr*expr
  | BoolOp  of bool_op*expr*expr
  | If      of expr*expr*expr
  | PrInt   of expr
  | Let     of string*expr*expr 
  | LetRec  of string*expr*expr 
  | Fun     of string*expr
  | App     of expr*expr
  | Seq     of expr*expr

(* définition du type des environnements*)
and env = (string*valeur) list

 (* définition du type pour les valeurs*) 
and valeur = 
  | VInt    of int 
  | VBool   of bool    
  | VFun    of string*expr*env*bool
  | VUnit

