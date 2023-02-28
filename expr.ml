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


let rec filtre expr recursif env = function
  | MNom nom -> [nom, begin match expr with
        | Fun (arg,e1) -> VFun (arg,e1,(modifier_env nom (VFun(arg,e1,env,recursif)) env),recursif)
        | _ -> eval expr env end]
  | MCouple (m1,m2) -> begin match expr with
        | CoupleExpr (e1,e2) -> filtre e1 recursif env m1 @ filtre e2 recursif env m2 
        | _ -> failwith "motif impossible"
        end
  | _ -> []

(* évaluation à grands pas *)
and eval e env = 
  if !Options.slow then (let _ = input_line stdin in ()); 
  if !Options.debug || !Options.slow then (Affichage.print_env env; Affichage.print_debug e);
  match e with
  | Const k            -> VInt k
  | BConst b           -> VBool b
  | Var cle            -> let v = begin match cle with
        | MNom nom -> trouver_env nom env
        | _       -> VUnit
      end in if !debug then (print_string "---->"; Affichage.affiche_val v); v
  | Unit               -> VUnit
  | CoupleExpr (e1,e2) -> VTuple (eval e1 env, eval e2 env)
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
  | BoolOp (op,e1,e2)  -> begin
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
  | Let (var,recursif,e1) -> VVar (filtre e1 recursif env var)
  | In (e1,e2)         -> eval e2 (match eval e1 env with
        | VVar l -> let rec aux env' = function
              | (s,v) :: l' -> aux (modifier_env s v env') l'
              | [] -> env'
            in aux env l 
        | _ -> env)
  | Fun (arg,e1)       -> VFun (arg,e1,env,false)
  | App (e1,e2)        -> begin match eval e1 env with
        | VFun (MUnit,corps,env',recursif) -> begin match eval e2 env with
              | VUnit -> eval corps env' 
              | _ -> failwith "Eval : App error (arg should have type Unit)" 
            end
        | VFun (arg,corps,env',recursif) -> eval corps ((match arg with
              | MNom nom -> modifier_env nom (eval e2 env)
              | _ -> fun x -> x) (if recursif then fusion_env env env' else env'))
        | _ -> failwith "Eval : App error (fun type)"
      end
  | Gseq (e1,e2)       -> eval e2 (match eval e1 env with
        | VVar l -> let rec aux env' = function
              | (s,v) :: l' -> aux (modifier_env s v env') l'
              | [] -> env'
            in aux env l 
        | _ -> env)
  | Seq (e1,e2)        -> begin match eval e1 env with
        | VUnit -> ()
        | _ -> print_string "[WARNING] : Seq (first expression should have type unit)" end; eval e2 env
  | Ref e1             -> incr next_ref ; let my_ref = !next_ref in
                          if my_ref < max_ref 
                            then (ref_memory.(my_ref) <- eval e1 env; VRef my_ref)
                            else failwith "Eval : Ref error (max limit of ref)"
  | ValRef e1          -> begin match eval e1 env with
        | VRef k -> ref_memory.(k)
        | _ -> failwith "Eval : ValRef error (arg should have type ref)"
      end
  | RefNew (e1,e2)     -> begin match e1 with
        | Var (MNom s) -> begin match trouver_env s env with 
              | VRef k -> ref_memory.(k) <- eval e2 env; VUnit
              | _ -> failwith "Eval : RefNew error (should have type ref)"
            end
        | _ -> failwith "Eval : RefNew error (should have type var)"
      end
  | Exn e1             -> begin match eval e1 env with
        | VInt k -> VExcep (k,false)
        | _ -> failwith "Eval : Exn error (arg should have type int)"
      end
  | Raise e1           -> begin match eval e1 env with 
        | VExcep (k,_) -> VExcep (k,true)
        | _ -> failwith "Eval : Raise error (arg should have type exn)"
      end
  | TryWith (e1,e2,e3) -> begin match e2 with
        | Exn arg -> begin match eval e1 env with
              | VExcep (m,true) -> begin match arg with
                    | Var var -> eval (App(Fun(var,e3),Const m)) env 
                    | Const k when k = m ->  eval e3 env
                    | _ -> failwith "Eval : TryWith error (exception arg should be var or int)"
                  end
              | v -> v
            end
        | _ -> failwith "Eval : TryWith error (with arg should be an exn)"
      end
  | Incr e1            -> begin match eval e1 env with
        | VRef k -> begin match ref_memory.(k) with
              | VInt n -> ref_memory.(k) <- VInt (n+1); VUnit
              | _ -> failwith "Eval : Incr error (ref should have type int)" 
            end
        | _ -> failwith "Eval : Incr error (arg should have type ref)"
      end
