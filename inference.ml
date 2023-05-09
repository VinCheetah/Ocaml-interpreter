open Types

(*
let rec (filtre_me : Types.problem) motif expr = match motif with
| MNom nom -> [(Var nom, find_type expr)]
| MCouple (m1,m2) -> (find_type expr, T)
| MCons (m1,m2) ->
| MEmptyList -> 
| MUnit -> 
| MExpr _ -> 
| MExcp motif -> 
| MNone -> let _ = *)



let give_next_var () = incr next_var; Var (MNom (string_of_int !next_var), None, false)


let inference = ref []

let add_inf x = inference := x :: !inference


let rec filtre_m e m global = match m with
  | MNom s -> add_inf (Var (MNom s, find_type e, global), None)
  | MNone -> ()
  | MCouple (m1,m2) -> begin match e with CoupleExpr (e1, e2) ->  filtre_m e1 m1 global; filtre_m e2 m2 global | Var a -> add_inf (Var (m, Var (a,None,false), global), None) end
  | MUnit -> add_inf (find_type e, T TUnit)
  | MExpr e1 -> add_inf (find_type e, find_type e1)
  | _ -> ()



and find_type = function
| Const _ -> T TInt
| BConst _ -> T TBool
| Var motif -> Var (motif, None, false)
| Unit -> T TUnit
| CoupleExpr (e1, e2) -> T (TTuple(find_type e1, find_type e2))
| ArithOp _ -> T TInt
| CompOp _ -> T TBool
| BoolOp _ -> T TBool
| If (_, e1, _) -> find_type e1
| PrInt _ -> T TInt
| Let (_,_,_,_,e2) -> find_type e2
| Fun (motif,e1) -> T (TFun (Var (motif,None,false), find_type e1))
| App (e1,e2) -> begin match find_type e1 with T TFun (_,t) -> t | Var (func,t1,b1) -> let alpha = give_next_var() in add_inf (Var (func,t1,b1), T (TFun (None, alpha))); alpha | _ -> None end
| Seq (e1,e2) -> find_type e2 
| Ref e1 -> T (TRef (find_type e1)) 
| ValRef e1 -> begin match find_type e1 with T (TRef t) -> t | _ -> None end
| RefNew _ -> T TUnit
| _ -> None




let rec inf = function
| ArithOp (op,e1,e2) -> add_inf (find_type e1, T TInt); add_inf (find_type e2, T TInt); inf e1; inf e2
| CompOp (op, e1, e2) -> add_inf (find_type e1, find_type e2); inf e1; inf e2
| BoolOp (op, e1, e2) -> add_inf (find_type e1, T TBool); add_inf (find_type e2, T TBool); inf e1; inf e2
| If (e1, e2, e3) -> add_inf (find_type e1, T TBool); add_inf (find_type e2, find_type e3); inf e1; inf e2; inf e3
| PrInt e1 -> add_inf (find_type e1, T TInt); inf e1
| Let (recursif,motif,e1,global,e2) -> filtre_m e1 motif global; inf e1; inf e2 
| Fun (motif, e1) -> inf e1
| App (e1, e2) -> add_inf (find_type e1, T (TFun (None, None))); (match e1 with 
  | Fun (motif, e3) -> filtre_m e1 motif false
  | Var func -> let alpha = give_next_var () in add_inf (Var (func, T (TFun (alpha, None)), false), None); add_inf (alpha, find_type e2)
  | _  -> ()); inf e1; inf e2
| Seq (e1, e2) -> add_inf (find_type e1, T TUnit); inf e1; inf e2
| Ref e1 -> inf e1
| ValRef e1 -> add_inf (find_type e1, T (TRef None)); inf e1
| RefNew (e1, e2) -> add_inf (find_type e1, T (TRef None)); (match e1 with 
  | Ref e3 -> add_inf (find_type e3, find_type e2)
  | _ -> ()); inf e1; inf e2
| _ -> ()






(*
print_prob (inf (ArithOp(Add,BConst false, Const 2)));
print_newline ();
print_prob (inf (If(CompOp (Eq, Ref (Const 3), Ref (Const 4)), ArithOp(Add,BConst false, Const 2), ArithOp(Add,Const 10, Const 2))));
print_newline ();
print_prob (inf (If(CompOp (Eq, Ref (Const 3), Ref (Const 4)), Let(false, MNom"x", Const 4, false, ArithOp(Add,Var (MNom "x"), Const 87)), ArithOp(Add,Const 10, Const 2))))
*)