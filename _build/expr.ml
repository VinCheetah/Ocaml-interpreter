(* d�finition des diff�rents types : tout est � reprendre et �tendre *)


(* définition du type pour les opérateurs de comparaison *)
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
  | VInt  of int 
  | VBool of bool    
  | VFun of string*expr*env
  | VUnit

  let empty_env = []

(* Ajoute une variable a un environnement en écrasant l'ancienne variable de meme nom si elle existe *)
let rec modifier_env cle valeur = function
  | (cle',valeur') :: env' -> if cle = cle' then (cle, valeur) :: env' else (cle', valeur') :: modifier_env cle valeur env'
  | [] -> [(cle,valeur)]


(* permet de récupérer la valeur d'une variable dans un environnement*)
let rec trouver_env nom env = match env with
  | (cle,valeur) :: env' -> if nom = cle then valeur else  trouver_env nom env'
  | [] -> print_string nom; print_newline (); failwith "Trouver_env : Variable absente"














  let affiche_comp = function
  | L  -> " < "
  | Le -> " <= "
  | G  -> " > "
  | Ge -> " >= "
  | Eq -> " = "
  | Ne -> " <> "

(* fonction d'affichage opérateurs arithmétiques *)
let affiche_arith = function
  | Add -> " + "
  | Min -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " mod "

(* fonction d'affichage opérateurs booléens *)
let affiche_bool = function
  | And -> " && "
  | Or  -> " || "
  | Not -> "not "


let rec affiche_expr e =
  let print_parenthese e = print_string "("; affiche_expr e ; print_string ")" in
  match e with
  | Const n            -> print_int n
  | BConst b           -> print_string (if b then "true" else "false")
  | Var x              -> print_string x
  | Unit               -> print_string "()"
  | ArithOp (op,e1,e2) -> (match e1 with
            | If _ -> print_parenthese
            | ArithOp (op',_,_) when op' = Add || op' = Min -> (match op with
                      | Div
                      | Mul -> print_parenthese
                      | _   -> affiche_expr)
            | _ -> affiche_expr) e1;
          print_string (affiche_arith op); (match e2 with
            | ArithOp (op',_,_) when op' = Add || op' = Min -> (match op with
                      | Div
                      | Mul -> print_parenthese
                      | _   -> affiche_expr)
            | _ -> affiche_expr) e2

  | CompOp (op,e1,e2) -> (match e1 with
            | BoolOp _ 
            | If _ -> print_parenthese
            | _ -> affiche_expr) e1;
            print_string (affiche_comp op);
            (match e2 with
            | BoolOp _ -> print_parenthese
            | _ -> affiche_expr) e2

  | BoolOp (op,e1,e2) -> begin match op with
            | Not -> print_string " not "; (match e1 with
                      | BConst _ -> affiche_expr
                      | _ -> print_parenthese) e1
            | And -> (match e1 with
                      | BoolOp (op',_,_) when op' = Or || op' = And -> print_parenthese
                      | If _ -> print_parenthese
                      | _ -> affiche_expr) e1;
                    print_string " && ";
                    (match e2 with
                      | BoolOp (op',_,_) when op' = Or -> print_parenthese
                      | _ -> affiche_expr) e2
            | Or -> (match e1 with
                      | If _ -> print_parenthese
                      | _ -> affiche_expr) e1;
                    print_string " || ";
                    affiche_expr e2
            end
  | If (c1,e1,e2) -> print_string "if " ; affiche_expr c1; print_string " then " ;affiche_expr e1; print_string " else "; affiche_expr e2
  | PrInt (e1)    -> print_string "prInt "; (match e1 with
            | Const _ -> affiche_expr
            | _ -> print_parenthese) e1  
  | Let (x,e1,e2) -> print_string "let "; print_string x; print_string " = "; affiche_expr e1; print_string " in "; affiche_expr e2
  | LetRec (x,e1,e2) -> print_string "let rec "; print_string x; print_string " = "; affiche_expr e1; print_string " in "; affiche_expr e2
  | Fun (arg,e1)  -> print_string "fun "; print_string arg; print_string" -> "; affiche_expr e1 
  | App (e1,e2)       -> affiche_expr e1; print_string " "; (match e2 with
            | Const _
            | BConst _
            | Var _ -> affiche_expr
            | _ -> print_parenthese) e2
  | Seq (e1,e2)   -> affiche_expr e1; print_string ";\n"; affiche_expr e2





  let affiche_val v = match v with
  | VInt k          -> print_int k
  | VBool b         -> print_string (if b then "true" else "false")
  | VFun (arg,e1,_) -> print_string "fun "; print_string arg; print_string" -> "; affiche_expr e1 
  | VUnit           -> print_string "()"
                      



let rec display_env env = match env with
  |[] -> print_newline ()
  |a::q -> let (s,v) = a in print_string s; affiche_val v; display_env q

let arith_op_eval op x y = match op with
  | Add -> x + y
  | Mul -> x * y
  | Min -> x - y 
  | Div -> x / y
  | Mod -> x mod y

let comp_op_eval op x y = match op with
  | L  -> x < y
  | Le -> x <= y
  | G  -> x > y
  | Ge -> x >= y
  | Eq -> x = y
  | Ne -> x <> y

let bool_op_eval op a b = match op with
  | And -> a && b
  | Or  -> a || b 
  | Not -> not a 


let debug e = print_string ("\nJe suis dans " ^ (match e with
| Const _ -> "Const"
| BConst _ -> "BConst"
| Var s -> "Var " ^ s
| Unit  -> "Unit"
| ArithOp _ -> "ArithOp"
| CompOp _  -> "CompOp"
| BoolOp  _ -> "BoolOp"
| If      _ -> "If"
| PrInt   _ -> "PrInt"
| LetRec   _ -> "LetRec"
| Fun     _ -> "Fun"
| App     _ -> "App"
| Let _ -> "Let"
| Seq _ -> "Seq"))

(* évaluation à grands pas *)
let rec eval e env = display_env env ;debug e; match e with
  | Const k            -> VInt k
  | BConst b           -> VBool b
  | Var cle            -> trouver_env cle env
  | Unit               -> VUnit
  | ArithOp (op,e1,e2) -> begin 
      match eval e1 env, eval e2 env with
        | VInt x, VInt y -> VInt (arith_op_eval op x y)
        | _ -> failwith "Eval : Arith error (mismatch type)"
      end
  | CompOp (op,e1,e2)  -> begin
      match eval e1 env, eval e2 env with
        | VInt x, VInt y -> VBool (comp_op_eval op x y) 
        | VBool a, VBool b -> VBool (comp_op_eval op a b)
        | _ -> failwith "Eval : Comp error (mismatch type)"
      end
  | BoolOp (op,e1,e2) -> begin
      match eval e1 env, eval e2 env with
        | VBool a, VBool b -> VBool (bool_op_eval op a b)
        | _ -> failwith "Eval : BoolOp (mismatch type)"
      end
  | If (c,e1,e2)       -> begin 
      match eval c env with 
        | VBool b -> begin
              match eval e1 env, eval e2 env with
                | VInt x, VInt y   -> if b then VInt x else VInt y  
                | VBool c, VBool d -> if b then VBool c else VBool d
                | _ -> failwith "Eval : If error (mismatch type)"
              end
        | _ -> failwith "Eval : If error (condition type)" 
      end
  | PrInt (e1)         -> begin
      match eval e1 env with
        | VInt x -> print_int x ; print_newline () ; VInt x
        | _ -> failwith "Eval : PrInt error (arg type)"
      end 
  | Let (nom,e1,e2)      -> eval e2 (modifier_env nom (eval e1 env) env)
  | LetRec (nom,e1,e2) -> display_env env; print_string "i am here"; begin match e1 with 
        | Fun (arg,e3) -> print_string "i am here";begin 
          
          match eval e2 (modifier_env nom (VFun (arg,e3,env)) env) with
                | VFun (arg',e4,env') when e3 = e4 ->print_string "i am here3"; display_env env'; eval e4 env'
                | valeur -> valeur
              end
        | exp -> eval exp env
    end
  | Fun (arg,e1)       -> VFun (arg,e1,env)
  | App (e1,e2) -> begin match eval e1 env with
        | VFun (arg,corps,env') -> eval corps (modifier_env arg (eval e2 env) env')
        | _ -> failwith "Eval : App error (fun type)"
      end
  | Seq (e1,e2) -> match eval e1 env with
        | VUnit -> eval e2 env
        | _ -> failwith "Eval : Seq error (first expression should have type unit)"
