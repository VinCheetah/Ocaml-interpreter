open Types
open Options
open Affichage
open Exceptions


let rec rename_motif = function
    | MNom m          -> MNom (rename_var m !index)
    | MCouple (m1,m2) -> MCouple (rename_motif m1, rename_motif m2)
    | MCons (m1, MEmptyList) -> MCons (rename_motif m1, MEmptyList)
    | MCons   (m1,m2) -> MCons (rename_motif m1, rename_motif m2)
    | m -> m 

let rec actu_index func global = function
    | MNom nom         -> index := func nom global !index
    | MCouple (m1, m2) -> actu_index func global m1; actu_index func global m2
    | MCons (m1, m2)   -> actu_index func global m1; actu_index func global m2
    | _                -> () 


let rec identifie_variables (e : expr) : expr = match e with
  | Var motif -> Var (rename_motif motif)
  | CoupleExpr (e1,e2) -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in CoupleExpr (e1', e2')
  | ArithOp (op,e1,e2) -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in ArithOp (op, e1', e2')
  | CompOp (op,e1,e2)  -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in CompOp (op, e1', e2')
  | BoolOp (op,e1,e2)  -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in BoolOp (op, e1', e2')
  | If (c1,e1,e2)      -> let c1' = identifie_variables c1 and e1' = identifie_variables e1 and e2' = identifie_variables e2 in If (c1', e1', e2')
  | PrInt e1           -> let e1' = identifie_variables e1 in PrInt (e1')
  | Let (recursif,var,e1,global,e2) -> (if recursif then actu_index new_var global var); let e1' = identifie_variables e1 in (if not recursif then actu_index new_var global var); let var' = rename_motif var and e2' = identifie_variables e2 in actu_index erase_var false var; Let (recursif, var', e1', global, e2')
  | Fun (arg,e1)       -> pre_cancelled := [] :: !pre_cancelled; actu_index new_var false arg; let arg' = rename_motif arg and e1' = identifie_variables e1 in actu_index erase_var false arg; cancellation (); Fun(arg', e1')
  | App (e1,e2)        -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in App (e1', e2') 
  | Seq (e1,e2)        -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in Seq (e1', e2')
  | Ref e1             -> let e1' = identifie_variables e1 in Ref e1'
  | ValRef e1          -> let e1' = identifie_variables e1 in ValRef e1'
  | RefNew (e1,e2)     -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in RefNew (e1', e2')
  | Exn e1             -> let e1' = identifie_variables e1 in Exn e1'
  | Raise e1           -> let e1' = identifie_variables e1 in Raise e1'
  | TryWith (e1,l)     -> let e1' = identifie_variables e1 in TryWith (e1', List.map (fun (motif,e) -> pre_cancelled := [] :: !pre_cancelled; actu_index new_var false motif; let motif' = rename_motif motif and e' = identifie_variables (Let (false,motif,e1,false,e)) in actu_index erase_var false motif; cancellation (); motif', e') l)
  | InDecr (e1,b)      -> let e1' = identifie_variables e1 in InDecr (e1',b)
  | Cons (e1,e2)       -> let e1' = identifie_variables e1 and e2' = identifie_variables e2 in Cons (e1', e2')
  | MatchWith (e1,l)   -> let e1' = identifie_variables e1 in MatchWith (e1', List.map (fun (motif,e) -> pre_cancelled := [] :: !pre_cancelled; actu_index new_var false motif; let motif' = rename_motif motif and e' = identifie_variables (Let (false,motif,e1,false,e)) in actu_index erase_var false motif; cancellation (); motif', e') l)
  | _ -> e



let rec filtre_motif_expr expr motif = add_inf (decompose_motif motif, find_type expr)





and decompose_motif = function
    | MNom m          -> identify_var m !correspondance
    | MCouple (m1,m2) -> T (TTuple (decompose_motif m1, decompose_motif m2))
    | MUnit           -> T TUnit
    | MExpr expr1     -> find_type expr1 
    | MCons (m1, MEmptyList) -> decompose_motif m1
    | MCons   (m1,m2) -> let alpha = give_next_var () and t = decompose_motif m2 in add_inf (decompose_motif m1, alpha); add_inf (T (TList alpha), t); t
    | MEmptyList      -> T (TList (give_next_prime ()))
    | _ -> None


and find_type = function
    | Const _                   -> T TInt
    | BConst _                  -> T TBool
    | Var motif                 -> decompose_motif motif
    | Unit                      -> T TUnit
    | CoupleExpr (expr1, expr2) -> T (TTuple(find_type expr1, find_type expr2))
    | ArithOp _                 -> T TInt
    | CompOp _                  -> T TBool
    | BoolOp _                  -> T TBool
    | If (_, expr1, _)          -> find_type expr1
    | PrInt _                   -> T TInt
    | Let (_,_,_,_,expr2)       -> find_type expr2
    | Fun (motif,expr1)         -> T (TFun (decompose_motif motif, find_type expr1))
    | App (expr1,expr2)         -> begin match find_type expr1 with 
        | T TFun (_,t) -> t 
        | Var (a,b,c)-> let alpha = give_next_var() in add_inf (Var (a,b,c), T (TFun (give_next_prime (), alpha))); alpha 
        | t            -> raise (TypeError (T (TFun (None,None)), t)) 
      end
    | Seq (expr1,expr2)         -> find_type expr2 
    | Ref expr1                 -> T (TRef (find_type expr1)) 
    | ValRef expr1              -> begin match find_type expr1 with
        | T (TRef t) -> t 
        | Var (a,b,c)      -> let alpha = give_next_var () in add_inf (Var (a,b,c), T (TRef alpha)); alpha
        | t -> raise (TypeError (T (TRef None), t)) end
    | RefNew _                  -> T TUnit
    | Cons (expr1,expr2)        -> T (TList (find_type expr1))
    | EmptyList                 -> T (TList (give_next_prime ()))
    | InDecr _                  -> T TUnit
    | Exn _                     -> T TExn
    | Raise _                   -> T TExn
    | _                         -> None




let rec inf expr = 
  if !Options.showinf then print_prob !inference; 
  match expr with
    | ArithOp (op,expr1,expr2)                -> add_inf (find_type expr1, T TInt); add_inf (find_type expr2, T TInt); inf expr1; inf expr2
    | CompOp (op, expr1, expr2)               -> add_inf (find_type expr1, find_type expr2); inf expr1; inf expr2
    | BoolOp (op, expr1, expr2)               -> add_inf (find_type expr1, T TBool); add_inf (find_type expr2, T TBool); inf expr1; inf expr2
    | If (expr1, expr2, expr3)                -> add_inf (find_type expr1, T TBool); add_inf (find_type expr2, find_type expr3); inf expr1; inf expr2; inf expr3
    | PrInt expr1                             -> add_inf (find_type expr1, T TInt); inf expr1
    | Let (recursif,motif,expr1,global,expr2) -> filtre_motif_expr expr1 motif; inf expr1; inf expr2;
    | Fun (motif, expr1)                      -> inf expr1
    | App (expr1, expr2)                      -> add_inf (find_type expr1, T (TFun (find_type expr2, give_next_prime ()))); inf expr1; inf expr2
    | Seq (expr1, expr2)                      -> inf expr1; inf expr2
    | Ref expr1                               -> inf expr1
    | ValRef expr1                            -> add_inf (find_type expr1, T (TRef (give_next_prime ()))); inf expr1
    | RefNew (expr1, expr2)                   -> add_inf (find_type expr1, T (TRef (find_type expr2))); inf expr1; inf expr2
    | Cons (expr1, expr2)                     -> let alpha = give_next_var () in add_inf (find_type expr1, alpha); add_inf (T (TList alpha), find_type expr2); inf expr1; inf expr2;
    | InDecr (expr1,_)                        -> add_inf (find_type expr1, T (TRef (T TInt)))
    | Exn expr1                               -> add_inf (find_type expr1, T TInt)
    | Raise expr1                             -> add_inf (find_type expr1, T TExn)
    | TryWith (expr1, liste)                  -> ()
    | MatchWith (expr1, liste)                -> ()
    | _                                       -> ()





