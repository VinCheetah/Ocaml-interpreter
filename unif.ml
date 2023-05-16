open Types
open Exceptions
open Inference
open Options
open Affichage


let ajout_inf = ref [];;




(*Renvoie true si x apparaît dans term*)
let rec appear v n t = 
  match t with
  | Var (v',n',_,_)    -> v = v' && n = n'
  | T (TFun (a,corps)) -> appear v n a || appear v n corps
  | T (TRef r)         -> appear v n r
  | T (TTuple (a,b))   -> appear v n a || appear v n b
  | T TList l          -> appear v n l
  | _ -> false
      
(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace (x, new_x) = function 
  | Var (v,n,t1,b1)       -> if (v, n) = x then (ajout_inf := (t1, new_x) :: !ajout_inf; new_x) else Var (v,n,replace (x,new_x) t1,b1)
  | T (TFun (arg, corps)) -> T (TFun ((replace (x, new_x) arg), replace (x, new_x) corps))
  | T (TRef r)            -> T (TRef (replace (x, new_x) r))
  | T (TTuple (a,b))      -> T (TTuple (replace (x, new_x) a, replace (x, new_x) b))  
  | T TList l             -> T (TList (replace (x,new_x) l))
  | a                     -> a



let rec replace_prime ((maxi, mini) : (int * t)) = function
    | Prime c when c = maxi -> mini
    | Var (v,n,t1,b1)       -> Var (v,n,replace_prime (maxi,mini) t1,b1)
    | T (TFun (arg, corps)) -> T (TFun ((replace_prime (maxi, mini) arg), replace_prime (maxi, mini) corps))
    | T (TRef r)            -> T (TRef (replace_prime (maxi, mini) r))
    | T (TTuple (a,b))      -> T (TTuple (replace_prime (maxi, mini) a, replace_prime (maxi, mini) b))  
    | T TList l             -> T (TList (replace_prime (maxi,mini) l))
    | a                     -> a



let compt = ref 0;;

let rec type_fixed = function
  | T TInt
  | T TBool
  | T TExn
  | T TUnit          -> true
  | T TRef t         -> type_fixed t
  | T (TFun (a,b)) 
  | T (TTuple (a,b)) -> type_fixed a && type_fixed b
  | T TList t        -> type_fixed t 
  | Var (_,_,t,_)    -> type_fixed t
  | Prime a          -> if occ_prime.(a) = 0 then print_string "0 prime"; occ_prime.(a) <= -1
  | _                -> false


let rec find_var v n = function
  | Var (v',n',t,b)  when v = v' && n = n' -> Var (v,n,t,b)
  | T TFun(a,_)      when appear v n a -> find_var v n a
  | T TFun(_,a)      when appear v n a -> find_var v n a
  | T TRef a         when appear v n a -> find_var v n a
  | T (TTuple (a,_)) when appear v n a -> find_var v n a
  | T (TTuple (_,a)) when appear v n a -> find_var v n a
  | T TList a        when appear v n a -> find_var v n a
  | _ -> None


let rec find_var_list v n = function
  | [] -> None
  | (a, _) :: _ when appear v n a -> find_var v n a
  | (_, a) :: _ when appear v n a -> find_var v n a
  | _ :: pb' -> find_var_list v n pb'


let rec fusion_type t1 t2 = 
  match t1, t2 with
  | t, None
  | None, t                                -> t, []
  | t, Prime a
  | Prime a, t                             -> t, [Prime a, t]
  | T (TFun (a1, b1)), T (TFun (a2,b2))    -> let t1,l1 = fusion_type a1 a2 and t2, l2 = fusion_type b1 b2 in T (TFun(t1, t2)), l1 @ l2
  | T (TTuple(a1, b1)), T (TTuple (a2,b2)) -> let t1,l1 = fusion_type a1 a2 and t2, l2 = fusion_type b1 b2 in T (TTuple(t1, t2)), l1 @ l2
  | T (TList t1), T (TList t2)             -> let t, l = fusion_type t1 t2 in T (TList t), l
  | t1, t2                                 -> t1, [(t1,t2)]






(*Implémente l'unification de deux termes*)
let rec unify pb =
  let update_inf pb = if !Options.showinf then (print_string "AJOUT : ";print_prob !ajout_inf); let pb' = !ajout_inf @ pb in ajout_inf := []; unify pb' in
  if !ajout_inf <> [] then update_inf pb else (incr compt; if !Options.showinf then print_prob pb;
  match pb with
  | [] -> []
  | x :: pb' -> begin
    match x with
    | Var (v, n, t, b), _ when type_fixed t || !compt >= compt_max -> if appear v n t then raise Not_unifyable else Var (v, n, t, b) :: (unify (List.map (fun (t1,t2) -> (replace ((v,n), t) t1), replace ((v,n), t) t2) pb')) 
    | _, Var (v, n, t, b) when type_fixed t || !compt >= compt_max -> if appear v n t then raise Not_unifyable else Var (v, n, t, b) :: (unify (List.map (fun (t1,t2) -> (replace ((v,n), t) t1), replace ((v,n), t) t2) pb'))
    | Prime a, Prime b -> occ_prime.(min a b) <- occ_prime.(min a b) + occ_prime.(max a b); occ_prime.(max a b) <- 0;
                          unify (List.map (fun (t1,t2) -> (replace_prime (max a b, Prime (min a b)) t1), replace_prime (max a b, Prime (min a b)) t2) pb')
    | t, Prime a
    | Prime a, t -> occ_prime.(a) <- 0; unify (List.map (fun (t1,t2) -> (replace_prime (a, t) t1, replace_prime (a, t) t2)) pb')
    | T (TTuple (a,b)), T (TTuple (c,d)) -> unify ((a,c) :: (b,d) :: pb') 
    | T (TFun (a,b)), T (TFun (c,d)) -> unify ((a,c) :: (b,d) :: pb')
    | T (TRef a), T (TRef b) -> unify ((a,b) :: pb')
    | T (TList a), T (TList b) -> unify ((a,b) :: pb')
    | T a, T b when a != b-> raise (TypeError (T a, T b))
    | T a, T b -> unify pb'
    | Var (v1,n1,t1,b1), Var (v2,n2,t2,b2) when v1 = v2 && n1 = n2 -> let t,l = fusion_type t1 t2 in unify (pb' @ l @ [(Var(v1, n1, t, b1 || b2), None)])
    | Var (v1,n1,t1,b1), Var (v2,n2,t2,b2) -> unify (pb' @ [(Var(v1,n1, Var (v2,n2,t2,b2), b1 || b2), None); Var (v2, n2, Var (v1,n1,t1,b1), b1 || b2), None])
    | None, Var (v,n,t,b)
    | Var (v,n,t,b), None -> unify (pb'  @ ([(Var (v,n,t,b), find_var_list v n pb')]))
    | t, Var (v,n,t1,b1)
    | Var (v,n,t1,b1), t -> let t2, l = fusion_type t1 t in unify ((Var (v, n, t2, b1), None) :: pb' @ l)
    | _, None -> unify pb'
    | None, _ -> unify pb'
  end)


