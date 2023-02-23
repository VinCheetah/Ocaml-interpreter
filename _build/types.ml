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

type motif =
  | NomM of string
  | NoneM
  | Couple of motif*motif


type name =
  | Nom of string 
  | None


 (* définition du type pour les expressions*) 
type expr =
  | Const   of int
  | BConst  of bool
  | Var     of motif
  | Unit
  | ArithOp of arith_op*expr*expr
  | CompOp  of comp_op*expr*expr
  | BoolOp  of bool_op*expr*expr
  | If      of expr*expr*expr
  | PrInt   of expr
  | Let     of motif*bool*expr*expr
  | Fun     of motif*expr
  | App     of expr*expr
  | Seq     of expr*expr
  | Ref     of expr
  | ValRef  of expr
  | RefNew  of expr*expr
  | Raise   of expr
  | TryWith of expr*expr*expr
  | Incr    of expr
  | CoupleExpr of expr*expr

(* définition du type des environnements*)
and env = (string*valeur) list

 (* définition du type pour les valeurs*) 
and valeur = 
  | VInt   of int 
  | VBool  of bool    
  | VFun   of motif*expr*env*bool
  | VUnit  of env
  | VRef   of valeur
  | VExcep of int*env
  | VTuple of valeur*valeur


