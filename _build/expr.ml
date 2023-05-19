open Types
open Options
open Affichage
open Exceptions


(* Ajoute une variable a un environnement en écrasant l'ancienne variable de meme nom si elle existe *)
let rec modifier_env cle valeur = function
  | (cle',valeur') :: env' -> if cle = cle' then (cle, valeur) :: env' else (cle', valeur') :: modifier_env cle valeur env'
  | [] -> [(cle,valeur)]


(* permet de récupérer la valeur d'une variable dans un environnement*)
let rec trouver_env nom env = match env with
  | (cle,(VFun(arg,corps,fun_env,true) as f_rec)) :: _ when cle = nom -> VFun (arg,corps,modifier_env cle f_rec fun_env,true)
  | (cle,valeur) :: env' -> if nom = cle then valeur else  trouver_env nom env'
  | [] -> raise (EvalError ("Variable inconnue : "^nom)) 


(* Effectue la fusion de deux environnements, si une variable est commune au deux alors celle de l'environnement ancien est écrasée *)
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



let rec filtre_val env v motif = fusion_env env (match motif with (* Fonction qui permet de filtrer les motifs et de renvoyer un environnement modifié*)
  | MNom nom -> [nom,v]
  | MCouple (m1,m2) -> begin match v with
        | VTuple (v1,v2) -> fusion_env (filtre_val env v1 m1) (filtre_val env v2 m2)
        | _ -> raise (EvalError "Filtre val : MCouple error (tuple expected)")
      end
  | MCons (m1,m2) -> begin match v with
        | VList (v1::v2) -> fusion_env (filtre_val env v1 m1) (filtre_val env (VList v2) m2)
        | _ -> raise (EvalError "Filtre val : MCons error (list expected)")
      end
  | MEmptyList -> begin match v with
        | VList [] -> []
        | _ -> raise (EvalError "Filtre val : MEmptyList error")
      end
  | MUnit -> begin match v with
        | VUnit -> []
        | _ -> raise (EvalError "Filtre val : MUnit error")
      end
  | MExpr e1 -> if v = (eval e1 env) then [] else raise (EvalError "Filtre val : MVal error")
  | _ -> [])


  (* Fonction qui permet de regarder si une expression correspond à un cas de matching *)
and is_matching expr env motif = try true, filtre expr false env motif with _ -> false,[]  


and filtre expr recursif env motif = fusion_env env (match motif with
  | MNom nom -> [nom, begin match expr with
        | Fun (arg,e1) -> VFun (arg,e1,env,recursif)
        | _ -> eval expr env end]
  | MCouple (m1,m2) -> begin match expr with
        | Var (MNom v) -> begin match trouver_env v env with 
              | VTuple (_) as t -> filtre_val env t motif
              | _ -> raise (EvalError "Filtre : MCouple error (tuple expected)")
            end
        | CoupleExpr (e1,e2) -> fusion_env (filtre e1 recursif env m1) (filtre e2 recursif env m2) 
        | _ -> raise (EvalError "Filtre : motif impossible")
      end
  | MCons (m1,m2) -> begin match expr with
        | Var (MNom v) -> begin match trouver_env v env with 
              | VList (_) as t -> filtre_val env t motif
              | _ -> raise (EvalError "Filtre : MCouple error (tuple expected)")
            end
        | Cons (e1,e2) -> fusion_env (filtre e1 recursif env m1) (filtre e2 recursif env m2)
        | _ -> raise (EvalError "Filtre : motif impossible")
      end
  | MEmptyList -> begin match expr with
        | Var (MNom v) -> begin match trouver_env v env with
              | VList [] -> []
              | _ -> raise (EvalError "Filtre : motif impossible")
            end
        | EmptyList -> []
        | _ -> raise (EvalError "Filtre : motif impossible")
      end
  | MUnit -> begin match eval expr env with
        | VUnit -> []
        | _ -> raise (EvalError "Filtre : motif impossible")
      end
  | MExpr _ -> filtre_val env (eval expr env) motif
  | MExcp motif -> begin match eval expr env with
        | VExcep (m,false) -> filtre_val env (VInt m) motif
        | _ -> raise (EvalError "Filtre : excep problem")
      end
  | MNone -> let _ = eval expr env in [])






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
  | CoupleExpr (e1,e2) -> VTuple (eval e1 env, eval e2 env) (* Traitement des couples *)
  | ArithOp (op,e1,e2) -> begin 
      match eval e2 env, eval e1 env with
        | VInt y, VInt x -> VInt (arith_op_eval op x y)
        | _ -> raise (EvalError "Operation arithmetique (mismatch types)")
      end
  | CompOp (op,e1,e2)  -> begin
      match eval e2 env, eval e1 env with
        | VInt y, VInt x -> VBool (comp_op_eval op x y) 
        | VBool b, VBool a -> VBool (comp_op_eval op a b)
        | _ -> raise (EvalError  "Comparaison error (uncomparable types)")
      end
  | BoolOp (op,e1,e2)  -> begin
      match eval e2 env, eval e1 env with
        | VBool b, VBool a -> VBool (bool_op_eval op a b)
        | _ -> raise (EvalError "Operation booleenne (mismatch types)")
      end
  | If (c,e1,e2)       -> begin (* Traitement des expressions if expr then expr else expr*)
      match eval c env with 
        | VBool b -> if b then eval e1 env else eval e2 env
        | _ -> raise (EvalError "Condition error (type should be bool)") 
      end
  | PrInt e1           -> begin
      match eval e1 env with
        | VInt x -> print_int x ; print_newline () ; VInt x
        | _ -> raise (EvalError "PrInt error (type should be int)")
      end 
  | Let (recursif,var,e1,global,e2) -> if not global then Options.in_dscolon := true;
                                       if global && !Options.in_dscolon then raise (EvalError "Declaration error (global in non-global declaration)");
                                       eval e2 (filtre e1 recursif env var) 
  | Fun (arg,e1)       -> VFun (arg,e1,env,false)
  | App (e1,e2) -> let arg = eval e2 env in  begin match eval e1 env with (* Traitement de l'application d'une expressions à une autre*)
        | VFun (motif,corps,fun_env,recursif) -> eval corps (filtre_val fun_env arg motif)
        | _ -> raise (EvalError "Application error (type should be fun)")
      end
  | Seq (e1,e2)        -> begin match eval e1 env with (* Evaluation des séquences d'expressions*)
        | VUnit -> ()
        | _ -> if !Options.warnings then print_string "[WARNING] Sequence (first expression should have type unit)" end; eval e2 env
  | Ref e1             -> incr next_ref ; let my_ref = !next_ref in (* Traitement des références *)
                          if my_ref < max_ref 
                            then (ref_memory.(my_ref) <- eval e1 env; VRef my_ref)
                            else raise (EvalError "Reference error (limite nombre reference atteinte)")
  | ValRef e1          -> begin match eval e1 env with
        | VRef k -> ref_memory.(k)
        | _ -> raise (EvalError "ValRef error (type should be ref)")
      end
  | RefNew (e1,e2)     -> begin match eval e1 env with
        | VRef k -> ref_memory.(k) <- eval e2 env; VUnit
        | _ -> raise (EvalError "RefNew error (type should be ref)")
      end
  | Exn e1             -> begin match eval e1 env with
        | VInt k -> VExcep (k,false)
        | _ -> raise (EvalError "Exception error (type should be int)")
      end
  | Raise e1           -> begin match eval e1 env with  (* Evaluation des expressions de type Raise expr*)
        | VExcep (k,_) -> VExcep (k,true)
        | _ -> raise (EvalError "Raise error (type should be exn)")
      end
  | TryWith (e1,l)     -> begin match eval e1 env with
        | VExcep (m,true) -> let rec aux = function
              | (MExcp (MExpr (Const n)),e2) :: l'  -> if n = m then eval e2 env else aux l' 
              | (MExcp motif,e2) :: l'-> let b,env' = is_matching (Const m) env motif in if b then eval e2 env' else aux l'
              | []                        -> VExcep (m,true)
              | _ -> raise (EvalError "TryWith error (case do not match an exception)")
            in aux l 
        | v -> v
      end
  | InDecr (e1,b)      -> begin match eval e1 env with (* Evaluation des expressions du type incr et decr sur des références*)
        | VRef k -> begin match ref_memory.(k) with
              | VInt n -> ref_memory.(k) <- VInt (n + if b then 1 else -1); VUnit
              | _ -> raise (EvalError "Incr error (type should be int ref)") 
            end
        | _ -> raise (EvalError "Incr error (type should be ref)")
      end
  | EmptyList          -> VList []
  | Cons (e1,e2)       -> begin match eval e2 env with (* Evaluations des expressions du type expr::expr au sein d'une liste*)
        | VList l -> VList (eval e1 env :: l)
        | _ -> raise (EvalError "Cons error (second argument should be list)")
      end
  | MatchWith (e1,l)   -> let rec aux = function (* Evaluation des expressions contennant un match with *)
        | (motif,e2) :: l' -> let b,env' = is_matching e1 env motif in if b then eval e2 env' else aux l'
        | [] -> raise (EvalError "Match failure")
      in aux l 