open Types
open Options
open Affichage


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
        | _       -> VUnit
      end in if !debug then (print_string "---->"; Affichage.affiche_val v); v
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
  | Let (var,recursif,e1) -> begin match var with 
        | Nom s -> VVal (s, begin match e1 with
              | Fun (arg,e1) -> VFun (arg,e1,env,recursif)
              | _ -> eval e1 env end)
        | _ -> eval e1 env
      end
  | In (e1,e2)         -> begin match eval e1 env with
        | VVal (s,v) -> eval e2 (modifier_env s v env)
        | _ -> failwith "Eval : In error (first expression should be a Let)"
      end
  | Fun (arg,e1)       -> VFun (arg,e1,env,false)
  | App (e1,e2)        -> begin match eval e1 env with
        | VFun (Uni,corps,env',recursif) -> begin match eval e2 env with
              | VUnit -> eval corps env' 
              | _ -> failwith "Eval : App error (arg should have type Unit)" 
            end
        | VFun (arg,corps,env',recursif) -> eval corps ((match arg with
              | Nom nom -> modifier_env nom (eval e2 env)
              | _ -> fun x -> x) (if recursif then fusion_env env env' else env'))
        | _ -> failwith "Eval : App error (fun type)"
      end
  | Gseq (e1,e2)       -> let v1 = eval e1 env in affiche_val v1; print_newline (); eval e2 (match eval e1 env with
        | VVal (nom,e1) -> (modifier_env nom e1 env)
        | _ -> env)
  | Seq (e1,e2)        -> begin match eval e1 env with
        | VUnit -> eval e2 env
        | _ -> failwith "Eval : Seq error (first expression should have type unit)"
      end
  | Ref e1             -> incr next_ref ; let my_ref = !next_ref in
                          if my_ref < max_ref 
                            then (ref_memory.(my_ref) <- eval e1 env; VRef my_ref)
                            else failwith "Eval : Ref error (max limit of ref)"
  | ValRef e1          -> begin match eval e1 env with
        | VRef k -> ref_memory.(k)
        | _ -> failwith "Eval : ValRef error (arg should have type ref)"
      end
  | RefNew (e1,e2)     -> begin match e1 with
        | Var (Nom s) -> begin match trouver_env s env with 
              | VRef k -> ref_memory.(k) <- eval e2 env; VUnit
              | _ -> failwith "Eval : RefNew error (should have type ref)"
            end
        | _ -> failwith "Eval : RefNew error (should have type var)"
      end
  | Exn e1              -> begin match eval e1 env with
        | VInt k -> VExcep (k,false)
        | _ -> failwith "Eval : Exn error (arg should have type int)"
      end
  | Raise e1           -> begin match eval e1 env with 
        | VExcep (k,_) -> VExcep (k,true)
        | _ -> failwith "Eval : Raise error (arg should have type exn)"
      end
  | TryWith (e1,arg,e2) -> begin match eval e1 env with
        | VExcep (m,true) -> begin match arg with
              | Var var -> eval (App(Fun(var,e2),Const m)) env 
              | _ -> begin match eval arg env with
                    | VInt n when n = m -> eval e2 env
                    | VInt k -> VExcep (m,true)
                    | _ -> failwith "Eval : TryWith error (exception arg should be var or int)"
                  end
            end
        | v -> v
      end
  | Incr e1            -> begin match eval e1 env with
        | VRef k -> begin match ref_memory.(k) with
              | VInt n -> ref_memory.(k) <- VInt (n+1); VUnit
              | _ -> failwith "Eval : Incr error (ref should have type int)" 
            end
        | _ -> failwith "Eval : Incr error (arg should have type ref)"
      end