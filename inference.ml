open Types
open Options
open Affichage
open Exceptions



(*
let identifie_variables = function
  | Var motif -> begin match motif with
    | MNom m -> index
  | e -> e *)


let rec actu_index func global = function
    | MNom nom         -> index := func nom global !index
    | MCouple (m1, m2) -> actu_index func global m1; actu_index func global m2
    | MCons (m1, m2)   -> actu_index func global m1; actu_index func global m2
    | _                -> () 



let rec filtre_motif_expr expr motif = add_inf (decompose_motif motif, find_type expr)

and decompose_motif = function
    | MNom m          -> identify_var m (give_next_prime ()) !index
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
    | Let (_,motif,_,global,expr2)   -> actu_index new_var global motif; let t = find_type expr2 in actu_index erase_var global motif; t
    | Fun (motif,expr1)         -> actu_index new_var false motif; let t = T (TFun (decompose_motif motif, find_type expr1)) in actu_index erase_var false motif; t
    | App (expr1,expr2)         -> begin match find_type expr1 with 
        | T TFun (_,t) -> t 
        | Var (a,b,c,d)-> actu_index new_var false (MNom a); let alpha = give_next_var() in add_inf (Var (a,b,c,d), T (TFun (give_next_prime (), alpha))); actu_index erase_var false (MNom a); alpha 
        | _            -> raise (TypeError (T (TFun (None,None)), None)) 
      end
    | Seq (expr1,expr2)         -> find_type expr2 
    | Ref expr1                 -> T (TRef (find_type expr1)) 
    | ValRef expr1              -> begin match find_type expr1 with
        | T (TRef t) -> t 
        | Var (a,b,c,d)      -> let alpha = give_next_var () in add_inf (Var (a,b,c,d), T (TRef alpha)); alpha
        | _ -> None end
    | RefNew _                  -> T TUnit
    | Cons (expr1,expr2)        -> T (TList (find_type expr1))
    | EmptyList                 -> T (TList (give_next_prime ()))
    | InDecr _                  -> T TUnit
    | _                         -> None




let rec inf expr = 
  if !Options.showinf then print_prob !inference; 
  match expr with
    | ArithOp (op,expr1,expr2)                -> add_inf (find_type expr1, T TInt); add_inf (find_type expr2, T TInt); inf expr1; inf expr2
    | CompOp (op, expr1, expr2)               -> add_inf (find_type expr1, find_type expr2); inf expr1; inf expr2
    | BoolOp (op, expr1, expr2)               -> add_inf (find_type expr1, T TBool); add_inf (find_type expr2, T TBool); inf expr1; inf expr2
    | If (expr1, expr2, expr3)                -> add_inf (find_type expr1, T TBool); add_inf (find_type expr2, find_type expr3); inf expr1; inf expr2; inf expr3
    | PrInt expr1                             -> add_inf (find_type expr1, T TInt); inf expr1
    | Let (recursif,motif,expr1,global,expr2) -> actu_index new_var global motif; filtre_motif_expr expr1 motif; inf expr1; inf expr2; actu_index erase_var global motif
    | Fun (motif, expr1)                      -> pre_cancelled := [] :: !pre_cancelled; actu_index new_var false motif; inf expr1; actu_index erase_var false motif; cancellation ()
    | App (expr1, expr2)                      -> add_inf (find_type expr1, T (TFun (give_next_prime (), give_next_prime ()))); begin match expr1 with 
        | Fun (motif, expr3) -> pre_cancelled := [] :: !pre_cancelled; actu_index new_var false motif; filtre_motif_expr expr1 motif; actu_index erase_var false motif; cancellation ()
        | Var func           -> let alpha = give_next_var () in add_inf (decompose_motif func, T (TFun (alpha, give_next_prime ()))); add_inf (alpha, find_type expr2)
        | _                  -> ()
      end; inf expr1; inf expr2
    | Seq (expr1, expr2)                      -> inf expr1; inf expr2
    | Ref expr1                               -> inf expr1
    | ValRef expr1                            -> add_inf (find_type expr1, T (TRef (give_next_prime ()))); inf expr1
    | RefNew (expr1, expr2)                   -> add_inf (find_type expr1, T (TRef (give_next_prime ()))); begin match expr1 with 
        | Ref expr3 -> add_inf (find_type expr2, find_type expr3)
        | Var motif -> add_inf (T (TRef (find_type expr2)), decompose_motif motif) 
        | _         -> ()
      end; inf expr1; inf expr2
    | Cons (expr1, expr2)                     -> let alpha = give_next_var () in add_inf (find_type expr1, alpha); add_inf (T (TList alpha), find_type expr2); inf expr1; inf expr2;
    | InDecr (expr1,_)                        -> add_inf (find_type expr1, T (TRef (T TInt)))
    | Exn expr1                               -> add_inf (find_type expr1, T TInt)
    | Raise expr1                             -> add_inf (find_type expr1, T TExn)
    | TryWith (expr1, liste)                  -> ()
    | MatchWith (expr1, liste)                -> ()
    | _                                       -> ()

