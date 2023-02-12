open Types
open Options
open Affichage


let empty_env = []

(* Ajoute une variable a un environnement en écrasant l'ancienne variable de meme nom si elle existe *)
let rec modifier_env cle valeur = function
  | (cle',valeur') :: env' -> if cle = cle' then (cle, valeur) :: env' else (cle', valeur') :: modifier_env cle valeur env'
  | [] -> [(cle,valeur)]


(* permet de récupérer la valeur d'une variable dans un environnement*)
let rec trouver_env nom env = match env with
  | (cle,valeur) :: env' -> if nom = cle then valeur else  trouver_env nom env'
  | [] -> failwith ("Trouver_env : Variable absente : " ^ nom) 


let rec fusion_env env_anc = function
  | (cle,valeur) :: env_act' -> fusion_env (modifier_env cle valeur env_anc) env_act'
  | [] -> env_anc




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
let rec eval e env = 
  if !Options.slow then (let _ = input_line stdin in ()); 
  if !Options.debug || !Options.slow then (Affichage.print_env env; Affichage.print_debug e);
  match e with
  | Const k            -> VInt k
  | BConst b           -> VBool b
  | Var cle            -> let v = begin match cle with
        | Nom nom -> trouver_env nom env
        | None    -> VUnit env
      end in if !debug then (print_string "---->"; Affichage.affiche_val v); v
  | Unit               -> VUnit env
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
        | VBool b -> (*begin
              match eval e1 env, eval e2 env with
                | VInt x, VInt y   -> if b then VInt x else VInt y  
                | VBool c, VBool d -> if b then VBool c else VBool d
                | _ -> failwith "Eval : If error (mismatch type)"
              end*) if b then eval e1 env else eval e2 env
        | _ -> failwith "Eval : If error (condition type)" 
      end
  | PrInt e1           -> begin
      match eval e1 env with
        | VInt x -> print_int x ; print_newline () ; VInt x
        | _ -> failwith "Eval : PrInt error (arg type)"
      end 
  (*| Let (nom,e1,e2)      -> eval e2 (modifier_env nom (eval e1 env) env)*)
  | Let (var,recursif,e1,e2) -> begin match var with
        | Nom nom -> eval e2 (modifier_env nom (if recursif then begin match e1 with
              | Fun (arg,e1) -> VFun (arg,e1,env,true)
              | _ -> eval e1 env end else eval e1 env) env)
        | None -> begin match eval e1 env with
              | VUnit env' -> eval e2 env'
              | _ -> eval e2 env
            end
      end
  | Fun (arg,e1)       -> VFun (arg,e1,env,false)
  | App (e1,e2)        -> begin match eval e1 env with
        | VFun (arg,corps,env',recursif) -> eval corps ((match arg with
              | Nom nom -> modifier_env nom (eval e2 env)
              | None -> fun x -> x) (if recursif then fusion_env env' env else env'))
        | _ -> failwith "Eval : App error (fun type)"
      end
  | Seq (e1,e2)        -> begin match eval e1 env with
        | VUnit env' -> eval e2 env'
        | _ -> failwith "Eval : Seq error (first expression should have type unit)"
      end
  | Ref e1             -> VRef (eval e1 env)
  | ValRef e1          -> begin match eval e1 env with
        | VRef v -> v
        | _ -> failwith "Eval : ValRef error (arg should have type ref)"
      end
  | RefNew (e1,e2)     -> begin match e1 with
        | Var (Nom s) when begin match trouver_env s env with | VRef r -> true | _ -> false end -> VUnit (modifier_env s (VRef (eval e2 env)) env)
        | _ -> eval e2 env
      end
  | Raise e1           -> begin match eval e1 env with 
        | VInt k -> VExcep k
        | _ -> failwith "Eval : Raise error (arg should have type int)"
      end
  | TryWith (e1,e2,e3)  -> begin match eval e2 env with
        | VInt n -> begin match (eval e1 env) with
              | VExcep m when n = m -> eval e3 env
              | v -> v
            end
        | _ -> failwith "Eval : TryWith error (exception's arg should have type int)"
      end