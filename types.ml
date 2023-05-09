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
  | MNom       of string
  | MCouple    of motif*motif
  | MNone
  | MUnit
  | MCons      of motif*motif
  | MEmptyList 
  | MExpr      of expr
  | MExcp      of motif


 (* définition du type pour les expressions*) 
and expr =
  | Const      of int
  | BConst     of bool
  | Var        of motif
  | Unit
  | CoupleExpr of expr*expr
(*| Fsd        of expr*bool*)
  | ArithOp    of arith_op*expr*expr
  | CompOp     of comp_op*expr*expr
  | BoolOp     of bool_op*expr*expr
  | If         of expr*expr*expr
  | PrInt      of expr
  | Let        of bool*motif*expr*bool*expr
  | Fun        of motif*expr
  | App        of expr*expr
  | Seq        of expr*expr
  | Ref        of expr
  | ValRef     of expr
  | RefNew     of expr*expr
  | Exn        of expr
  | Raise      of expr
  | TryWith    of expr* (motif*expr) list
  | InDecr     of expr*bool
  | EmptyList
  | Cons       of expr*expr
  | MatchWith  of expr* (motif*expr) list


(* définition du type des environnements*)
and env = (string*valeur) list


 (* définition du type pour les valeurs*) 
and valeur = 
  | VInt       of int 
  | VBool      of bool    
  | VFun       of motif*expr*env*bool
  | VUnit      
  | VRef       of int
  | VTuple     of valeur*valeur
  | VList      of valeur list
  | VExcep     of int*bool (*Le booléen permet de savoir si l'exception est levée ou juste renvoyée*)



let empty_env = []
let max_ref = 1234
let ref_memory = Array.make max_ref VUnit
let next_ref = ref 0  
let next_var = ref 0



type types =
  | TInt
  | TBool
  | TUnit
  | TFun of t*t
  | TRef of t
  | TTuple of t*t
  | TList of t


and t =
  | Var of motif*t*bool
  | T of types 
  | None

type problem = (t * t) list




let rec print_type = function
  | Var (s,t,_) -> begin match s with 
    | MNom s -> s ^ "(" ^ print_type t ^ ") "
    | _ -> "motif" end
  | None -> "None"
  | T c -> begin match c with
    | TInt -> "Int"
    | TBool -> "Bool"
    | TFun (arg,corps) -> (print_type arg) ^ " -> " ^ (print_type corps)
    | TUnit -> "Unit"
    | TRef r -> "Ref : " ^ (print_type r)
    | TTuple (a,b) -> (print_type a) ^ " * " ^ (print_type b) 
    | TList _ -> "List" end



let rec print_prob = function
| [] -> print_newline ()
| (a,b):: l' -> print_string (print_type a);
    print_string " ----- "; 
    print_string (print_type b); print_string "\n"; print_prob l' 
;;