open Types
open Exceptions
open Inference
open Options
open Affichage


let ajout_inf = ref [];;




(*Renvoie true si x apparaît dans term*)
let rec appear v t = 
  match t with
  | Var (v',_,_)   -> v = v'
  | T (TTuple (a,b))
  | T (TFun (a,b)) -> appear v a || appear v corps
  | T (TRef r)
  | T TList l      -> appear v l
  | _              -> false
      
(*Effectue la substitution sigma(term) = term[new_x/x] *)
let rec replace (x, new_x) = function 
  | Var (v,t1,b1)         -> if v = x then (ajout_inf := (t1, new_x) :: !ajout_inf; new_x) else Var (v,replace (x,new_x) t1,b1)
  | T (TFun (arg, corps)) -> T (TFun ((replace (x, new_x) arg), replace (x, new_x) corps))
  | T (TRef r)            -> T (TRef (replace (x, new_x) r))
  | T (TTuple (a,b))      -> T (TTuple (replace (x, new_x) a, replace (x, new_x) b))  
  | T TList l             -> T (TList (replace (x,new_x) l))
  | a                     -> a



let rec replace_prime (maxi, mini) = function
    | Prime c when c = maxi -> mini
    | Var (v,t1,b1)         -> Var (v,replace_prime (maxi,mini) t1,b1)
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
  | T (TFun (a,b)) 
  | T (TTuple (a,b)) -> type_fixed a && type_fixed b
  | T TRef t 
  | T TList t 
  | Var (_,t,_)      -> type_fixed t
  | Prime a          -> if occ_prime.(a) = 0 then print_string ("0 prime : '"^Char.escaped (Char.chr (a+96))^"\n"); occ_prime.(a) <= -1
  | _                -> false


let rec find_var v = function
  | Var (v',t,b)  when v = v' -> Var (v,t,b)
  | T TFun(a,_)      when appear v a
  | T TFun(_,a)      when appear v a
  | T TRef a         when appear v a
  | T (TTuple (a,_)) when appear v a
  | T (TTuple (_,a)) when appear v a
  | T TList a        when appear v a -> find_var v a
  | _ -> None


let rec find_var_list v = function
  | [] -> None
  | (a, _) :: _ when appear v a -> find_var v a
  | (_, a) :: _ when appear v a -> find_var v a
  | _ :: pb' -> find_var_list v pb'


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
    | Var (v, t, b), _ when type_fixed t || !compt >= compt_max -> Var (v, t, b) :: (unify (List.map (fun (t1,t2) -> replace_prime (prime_var v !correspondance, t) (replace (v, t) t1), replace_prime (prime_var v !correspondance, t) (replace (v, t) t2)) pb')) 
    | _, Var (v, t, b) when type_fixed t || !compt >= compt_max -> Var (v, t, b) :: (unify (List.map (fun (t1,t2) -> (replace (v, t) t1), replace (v, t) t2) pb'))
    | Prime a, Prime b -> occ_prime.(min a b) <- occ_prime.(min a b) + occ_prime.(max a b); occ_prime.(max a b) <- 0;
                          unify (List.map (fun (t1,t2) -> (replace_prime (max a b, Prime (min a b)) t1), replace_prime (max a b, Prime (min a b)) t2) pb')
    | Var (_,t,_), Prime a
    | Prime a, Var (_,t,_)
    | t, Prime a
    | Prime a, t -> occ_prime.(a) <- 0; unify (List.map (fun (t1,t2) -> (replace_prime (a, t) t1, replace_prime (a, t) t2)) pb')
    | T (TTuple (a,b)), T (TTuple (c,d)) -> unify ((a,c) :: (b,d) :: pb') 
    | T (TFun (a,b)), T (TFun (c,d)) -> unify ((a,c) :: (b,d) :: pb')
    | T (TRef a), T (TRef b) -> unify ((a,b) :: pb')
    | T (TList a), T (TList b) -> unify ((a,b) :: pb')
    | T a, T b when a != b-> raise (TypeError (T a, T b))
    | T a, T b -> unify pb'
    | Var (v1,t1,b1), Var (v2,t2,b2) when v1 = v2 -> let t,l = fusion_type t1 t2 in unify (pb' @ l @ [(Var(v1, t, b1 || b2), None)])
    | Var (v1,t1,b1), Var (v2,t2,b2) -> unify (pb' @ [(Var(v1, Var (v2,t2,b2), b1 || b2), None); Var (v2, Var (v1,t1,b1), b1 || b2), None])
    | None, Var (v,t,b)
    | Var (v,t,b), None -> unify (pb'  @ ([(Var (v,t,b), find_var_list v pb')]))
    | t, Var (v,t1,b1)
    | Var (v,t1,b1), t -> let t2, l = fusion_type t1 t in unify ((Var (v, t2, b1), None) :: pb' @ l)
    | _, None -> unify pb'
    | None, _ -> unify pb'
  end)


