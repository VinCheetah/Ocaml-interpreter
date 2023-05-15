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
  | MNom of string
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
  | Var of string * int * t * bool
  | T of types 
  | None

type problem = (t * t) list










let index : (string * int ref * bool) list ref  = ref [];;

let inference : (t * t) list ref = ref [];;

let add_inf x = inference := !inference @ [x]

let pre_cancelled : (string * int) list list ref = ref [];; 

let cancelled : (string * int) list ref = ref [];;

let rec add x = function
  | [] -> [x]
  | a :: l' -> a :: (if a = x then l' else add x l')


let rec print_cancelled = function
  | [] -> print_newline ()
  | (a, r) :: l' -> print_string (a^" -- "); print_int r; print_newline (); print_cancelled l'


let cancellation () = if !pre_cancelled = [] then print_string "suspicious\n" 
                      else (cancelled := !cancelled @ (List.hd !pre_cancelled); pre_cancelled := List.tl !pre_cancelled); 
                      if !Options.showinf then print_cancelled !cancelled

let rec new_var x global = function
  | [] -> if List.mem (x, 1) !cancelled then new_var x global [(x, ref 1, global)] 
          else (if !pre_cancelled <> [] then pre_cancelled := add (x, 1) (List.hd !pre_cancelled) :: (List.tl !pre_cancelled); [(x, ref 1, global)])
  | (a, r, g) :: l' when a = x -> incr r; 
                                  if List.mem (x, !r) !cancelled then (if !Options.showinf then print_string "found cancelled\n"; new_var x global ((a,r,g)::l')) 
                                  else (if !Options.showinf then (print_string ("not cancelled : "^a^" -- "); print_int !r; print_newline ());
                                        if !pre_cancelled <> [] then pre_cancelled := add (x, !r) (List.hd !pre_cancelled) :: (List.tl !pre_cancelled);
                                        (a, r, global) :: l')
  | t :: l' -> t :: (new_var x global l')


let rec erase_var x global l = match l with
  | [] -> []
  | (a, r, g) :: l' when a = x -> if !r <= 1 then l' else (decr r; if List.mem (a, !r) !cancelled then erase_var x global l else (a, r, g) :: l')
  | t :: l' -> t :: (erase_var x global l')


let rec identify_var x t = function
  | [] -> if !Options.showinf || !Options.debug then print_string ("Unindexed : "^x^"\n"); Var (x, 0, t, false) 
  | (a, r, g) :: l' when a = x -> Var (a, !r, t, g)
  | _ :: l' -> identify_var x t l'



let give_next_var () = incr next_var; Var ("aux", !next_var, None, false)

let compt_max = 300;;
