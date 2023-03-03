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




(*
let rec check_corps e env arg = 
  match e with
  | Const k            -> ()
  | BConst b           -> ()
  | Var cle            -> begin match cle with
        | MNom nom when nom <> arg -> let _ = trouver_env nom env in ()
        | _       -> ()
          end
  | Unit               -> ()
  | CoupleExpr (e1,e2) -> check_corps e1 env arg; check_corps e2 env arg
  | ArithOp (op,e1,e2) -> check_corps e1 env arg; check_corps e2 env arg
  | CompOp (op,e1,e2)  -> check_corps e1 env arg; check_corps e2 env arg
  | BoolOp (op,e1,e2)  -> check_corps e1 env arg; check_corps e2 env arg
  | If (c,e1,e2)       -> check_corps c env arg ; check_corps e1 env arg; check_corps e2 env arg
  | PrInt e1           -> check_corps e1 env arg
  | Let (var,recursif,e1) -> check_corps e1 env arg;
  | In (e1,e2)         -> check_corps e1 env arg; check_corps e2 (match e1 with
        | Let (motif,recursif,e1) -> let rec aux = function
                | MNom nom -> [nom,VUnit]
                | MCouple (m1,m2) -> (aux m1) @ (aux m2)
                | _ -> []
          in env @ (aux motif)
        | _ -> env) arg
  | Fun (motif,e1)      -> begin match motif with 
        | MNom arg'-> check_corps e1 (modifier_env arg VUnit env)  arg'
        | _ -> check_corps e1 env arg
        end
  | App (e1,e2)        -> check_corps e1 env arg; check_corps e2 env arg
  | Gseq (e1,e2)       -> failwith "Check Corps : Gseq error"
  | Seq (e1,e2)        -> check_corps e1 env arg; check_corps e2 env arg
  | Ref e1             -> check_corps e1 env arg
  | ValRef e1          -> check_corps e1 env arg
  | RefNew (e1,e2)     -> check_corps e1 env arg; check_corps e2 env arg
  | Exn e1             -> check_corps e1 env arg
  | Raise e1           -> check_corps e1 env arg
  | TryWith (e1,e2,e3) -> check_corps e1 env arg ; check_corps e2 env arg; check_corps e3 env arg
  | Incr e1            -> check_corps e1 env arg







let rec filtre expr recursif env = function
  | MNom nom -> [nom, begin match expr with
        | Fun (arg,e1) -> (match arg with 
              | MNom nom -> check_corps e1 env nom 
              | _ -> check_corps e1 ((if recursif then [nom,VUnit] else []) @ env) ""); VFun (arg,e1,(modifier_env nom (VFun(arg,e1,env,recursif)) env),recursif)
        | _ -> eval expr env end]
  | MCouple (m1,m2) -> begin match expr with
        | CoupleExpr (e1,e2) -> filtre e1 recursif env m1 @ filtre e2 recursif env m2 
        | _ -> failwith "motif impossible"
        end
  | _ -> []*)


let rec filtre_val v = function
  | MNom nom -> [nom,v]
  | MCouple (m1,m2) -> begin match v with
        | VTuple (v1,v2) -> fusion_env (filtre_val v1 m1) (filtre_val v2 m2)
        | _ -> failwith "Filtre val : MCouple error (tuple expected)"
      end
  | _ -> []

let rec filtre expr recursif env var = fusion_env env (match var with
  | MNom nom -> [nom, begin match expr with
        | Fun (arg,e1) -> VFun (arg,e1,(if recursif then modifier_env nom (VFun(arg,e1,env,recursif)) env else env),recursif)
        | _ -> eval expr env end]
  | MCouple (m1,m2) as m -> begin match expr with
        | Var (MNom v) -> begin match trouver_env v env with 
              | VTuple (v1,v2) as t -> filtre_val t m
              | _ -> failwith "Filtre : MCouple error (tuple expected)"
            end
        | CoupleExpr (e1,e2) -> fusion_env (filtre e1 recursif env m1) (filtre e2 recursif env m2) 
        | _ -> failwith "motif impossible"
        end
  | _ -> [])






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
  | Let (recursif,var,e1,global,e2) -> if not global then Options.in_dscolon := true; if global && !Options.in_dscolon then failwith "Eval : Let error (in_dscolon)" else
                                       
                                      eval e2 (filtre e1 recursif env var)
  | Fun (arg,e1)       -> VFun (arg,e1,env,false)
  | App (e1,e2)        -> begin match eval e1 env with
        | VFun (MUnit,corps,env',recursif) -> (match eval e2 env with
              | VUnit -> eval corps (if recursif then fusion_env env env' else env')
              | _ -> failwith "Eval : App error (arg should have type Unit)")
        | VFun (MNom nom,corps,env',recursif) -> eval corps (modifier_env nom (eval e2 env) (if recursif then fusion_env env env' else env'))
        | VFun (MNone,corps,env',recursif) -> eval corps (if recursif then fusion_env env env' else env')
        | _ -> failwith "Eval : App error (fun type)"
      end
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
  | EmptyList -> VList []
  | Cons (e1,e2) -> match eval e2 env with
        | VList l -> VList (eval e1 env :: l)
        | _ -> failwith "Eval : Cons error (second argument should be list)"









