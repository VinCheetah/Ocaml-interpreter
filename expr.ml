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
and env = Env of (string*expr*env) list
 (* définition du type pour les valeurs*) 
and valeur = 
  | VInt  of int 
  | VBool of bool    
  | VFun of string*expr*env
  | VUnit

let empty_env = Env []

(* Ajoute une variable a un environnement en écrasant l'ancienne variable de meme nom si elle existe *)
let rec modifier_env cle ex en = function
  | Env env -> begin match env with
  | (cle',ex',en') :: env' -> if cle = cle' then (cle, ex', en') :: env' else (cle', ex', en') :: modifier_env cle ex en (Env env')
  | [] -> [(cle,ex,en)]
  end

(* permet de récupérer la valeur d'une variable dans un environnement*)
let rec trouver_env nom = function
  | Env env -> begin match env with
  | (cle,ex,en) :: env' -> if nom = cle then (ex,en) else  trouver_env nom (Env env')
  | [] -> print_string nom; failwith "Trouver_env : Variable absente"
  end

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


(* évaluation à grands pas *)
let rec eval e (env:env) = match e with
  | Const k            -> VInt k
  | BConst b           -> VBool b
  | Var cle            -> let ex, en = trouver_env cle env in eval ex en
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
  | Let (nom,e1,e2)      -> eval e2 (Env (modifier_env nom  e1 env env))
  | LetRec (nom,e1,e2) -> eval e2 (Env (modifier_env nom  e1 (Env (modifier_env nom e1 env env)) env))
  | Fun (arg,e1)       -> VFun (arg,e1,env)
  | App (e1,e2) -> begin match eval e1 env with
        | VFun (arg,corps,env') -> eval corps (Env (modifier_env arg e2 env env'))
        | _ -> failwith "Eval : App error (fun type)"
      end
  | Seq (e1,e2) -> match eval e1 env with
        | VUnit -> eval e2 env
        | _ -> failwith "Eval : Seq error (first expression should have type unit)"



        (*let rec f env_rec = modifier_env nom (eval e1 (f env_rec)) env_rec in eval e2 (f env)*)