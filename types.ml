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

type name =
  | Nom of string 
  | None
  | Uni


 (* définition du type pour les expressions*) 
type expr =
  | Const   of int
  | BConst  of bool
  | Var     of name
  | Unit
  | ArithOp of arith_op*expr*expr
  | CompOp  of comp_op*expr*expr
  | BoolOp  of bool_op*expr*expr
  | If      of expr*expr*expr
  | PrInt   of expr
  | Let     of name*bool*expr
  | In      of expr*expr
  | Fun     of name*expr
  | App     of expr*expr
  | Gseq    of expr*expr
  | Seq     of expr*expr
  | Ref     of expr
  | ValRef  of expr
  | RefNew  of expr*expr
  | Exn     of expr
  | Raise   of expr
  | TryWith of expr*expr*expr
  | Incr    of expr

(* définition du type des environnements*)
and env = (string*valeur) list

 (* définition du type pour les valeurs*) 
and valeur = 
  | VInt   of int 
  | VBool  of bool    
  | VFun   of name*expr*env*bool
  | VUnit
  | VRef   of int
  | VVal   of string*valeur
  | VExcep of int*bool (*Le booléen permet de savoir si l'exception est levée ou juste renvoyée*)



let empty_env = []
let max_ref = 1234
let ref_memory = Array.make max_ref VUnit
let next_ref = ref 0  