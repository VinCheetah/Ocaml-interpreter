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



type types =
  | TInt
  | TBool
  | TFun of t*t
  | TUnit
  | TRef of t
  | TTuple of t*t
  | TList of types
  | TExcep


and t =
  | Var of motif
  | Fun of string*t list
  | T of types 
  | None

type problem = (t * t) list

(*
let rec (filtre_me : Types.problem) motif expr = match motif with
| MNom nom -> [(Var nom, find_type expr)]
| MCouple (m1,m2) -> (find_type expr, T)
| MCons (m1,m2) ->
| MEmptyList -> 
| MUnit -> 
| MExpr _ -> 
| MExcp motif -> 
| MNone -> let _ = *)




let rec filtre_m = function
  | MNom s -> Var (MNom s)
  | MNone -> None
  | MCouple (m1,m2) -> T (TTuple (filtre_m m1, filtre_m m2))
  | MUnit -> T TUnit
(*| MCons (m1, m2) -> let t = filtre m1 in TList t *)
  | MExpr e1 -> find_type e1
  | _ -> T TBool



and find_type = function
| Const _ -> T TInt
| BConst _ -> T TBool
| Var motif -> filtre_m motif
| Unit -> T TUnit
| CoupleExpr (e1, e2) -> T (TTuple(find_type e1, find_type e2))
| ArithOp _ -> T TInt
| CompOp _ -> T TBool
| BoolOp _ -> T TBool
| If (_, e1, _) -> find_type e1
| PrInt _ -> T TInt
| Let (_,_,_,_,e2) -> find_type e2
| Fun (motif,e1) -> T (TFun (Var motif, find_type e1))
| App (e1,e2) -> begin match find_type e1 with T TFun (_,t) -> t | _ -> None end
| Seq (e1,e2) -> find_type e2 
| Ref e1 -> T (TRef (find_type e1)) 
| ValRef e1 -> begin match find_type e1 with T (TRef t) -> t | _ -> None end
| RefNew _ -> T TUnit
| _ -> None


let rec arg_fun l = function
| Fun (arg, e1) -> arg_fun (Var arg :: l) e1
| e -> Fun (l, e) 


let rec inf = function
| Const _ -> []
| BConst _ -> []
| Var _ -> []
| Unit -> []
| CoupleExpr _ -> []
| ArithOp (op,e1,e2) -> (find_type e1, T TInt) :: (find_type e2, T TInt) :: inf e1 @ inf e2
| CompOp (op, e1, e2) -> (find_type e1, find_type e2) :: inf e1 @ inf e2
| BoolOp (op, e1, e2) -> (find_type e1, T TBool) :: (find_type e2, T TBool) :: inf e1 @ inf e2
| If (e1, e2, e3) -> (find_type e1, T TBool) :: (find_type e2, find_type e3) :: inf e1 @ inf e2 @ inf e3
| PrInt e1 -> (find_type e1, T TInt) :: inf e1
| Let (recursif,motif,e1,global,e2) -> (match e1 with Fun _ -> arg_fun [] e1 | _ -> (filtre_m motif, find_type e1)) :: inf e1 @ inf e2 
| Fun (motif, e1) -> inf e1
| App (e1, e2) -> (find_type e1, T (TFun (None, None))) :: (match e1 with 
  | Fun (motif, _) -> (find_type e2, filtre_m motif)
  | _  -> (None, None)) :: inf e1 @ inf e2
| Seq (e1, e2) -> (find_type e1, T TUnit) :: inf e1 @ inf e2
| Ref e1 -> inf e1
| ValRef e1 -> (find_type e1, T (TRef None)) :: inf e1
| RefNew (e1, e2) -> (find_type e1, T (TRef None)) :: (match e1 with 
  | Ref e3 -> (find_type e3, find_type e2)
  | _ -> (None, None)) :: inf e1 @ inf e2
| Exn _ 
| Raise _      
| TryWith _ 
| InDecr _ 
| EmptyList
| Cons _ 
| MatchWith _ -> []


let rec print_prob = function
| [] -> ()
| (a,b):: l' -> print_string (match a with
  | Var s -> "motif"
  | Fun _ -> "Fun2"
  | None -> "None"
  | T c -> begin match c with
    | TInt -> "Int"
    | TBool -> "Bool"
    | TFun _ -> "Fun"
    | TUnit -> "Unit"
    | TRef _ -> "Ref"
    | TTuple _ -> "Tuple"
    | TList _ -> "List"
    | TExcep _ -> "Exn" end);
    print_string " ----- "; 
    print_string (match b with
  | Var s -> "motif"
  | Fun _ -> "Fun2"
  | None -> "None"
  | T c -> begin match c with
    | TInt -> "Int"
    | TBool -> "Bool"
    | TFun _ -> "Fun"
    | TUnit -> "Unit"
    | TRef _ -> "Ref"
    | TTuple _ -> "Tuple"
    | TList _ -> "List"
    | TExcep _ -> "Exn" end); print_string "\n"; print_prob l' 
;;


print_prob (inf (ArithOp(Add,BConst false, Const 2)));
print_newline ();
print_prob (inf (If(CompOp (Eq, Ref (Const 3), Ref (Const 4)), ArithOp(Add,BConst false, Const 2), ArithOp(Add,Const 10, Const 2))));
print_newline ();
print_prob (inf (If(CompOp (Eq, Ref (Const 3), Ref (Const 4)), Let(false, MNom"x", Const 4, false, ArithOp(Add,Var (MNom "x"), Const 87)), ArithOp(Add,Const 10, Const 2))))