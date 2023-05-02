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




let rec filtre_m = function
  | MNom s -> Var (MNom s)
  | MNone -> None
  | MCouple (m1,m2) -> T (TTuple (filtre_m m1, filtre_m m2))
  | MUnit -> T TUnit
(*| MCons (m1, m2) -> let t = filtre m1 in TList t *)
  | MExpr e1 -> find_type e1
  | _ -> T TBool



and find_type = function
| Const _ -> T TInt
| BConst _ -> T TBool
| Var motif -> filtre_m motif
| Unit -> T TUnit
| CoupleExpr (e1, e2) -> T (TTuple(find_type e1, find_type e2))
| ArithOp _ -> T TInt
| CompOp _ -> T TBool
| BoolOp _ -> T TBool
| If (_, e1, _) -> find_type e1
| PrInt _ -> T TInt
| Let (_,_,_,_,e2) -> find_type e2
| Fun (motif,e1) -> T (TFun (Var motif, find_type e1))
| App (e1,e2) -> begin match find_type e1 with T TFun (_,t) -> t | _ -> None end
| Seq (e1,e2) -> find_type e2 
| Ref e1 -> T (TRef (find_type e1)) 
| ValRef e1 -> begin match find_type e1 with T (TRef t) -> t | _ -> None end
| RefNew _ -> T TUnit
| _ -> None




let rec inf = function
| Const _ -> []
| BConst _ -> []
| Var _ -> []
| Unit -> []
| CoupleExpr _ -> []
| ArithOp (op,e1,e2) -> (find_type e1, T TInt) :: (find_type e2, T TInt) :: inf e1 @ inf e2
| CompOp (op, e1, e2) -> (find_type e1, find_type e2) :: inf e1 @ inf e2
| BoolOp (op, e1, e2) -> (find_type e1, T TBool) :: (find_type e2, T TBool) :: inf e1 @ inf e2
| If (e1, e2, e3) -> (find_type e1, T TBool) :: (find_type e2, find_type e3) :: inf e1 @ inf e2 @ inf e3
| PrInt e1 -> (find_type e1, T TInt) :: inf e1
| Let (recursif,motif,e1,global,e2) -> (filtre_m motif, find_type e1) :: inf e1 @ inf e2 
| Fun (motif, e1) -> inf e1
| App (e1, e2) -> (find_type e1, T (TFun (None, None))) :: (match e1 with 
  | Fun (motif, _) -> (find_type e2, filtre_m motif)
  | _  -> (None, None)) :: inf e1 @ inf e2
| Seq (e1, e2) -> (find_type e1, T TUnit) :: inf e1 @ inf e2
| Ref e1 -> inf e1
| ValRef e1 -> (find_type e1, T (TRef None)) :: inf e1
| RefNew (e1, e2) -> (find_type e1, T (TRef None)) :: (match e1 with 
  | Ref e3 -> (find_type e3, find_type e2)
  | _ -> (None, None)) :: inf e1 @ inf e2
| Exn _ 
| Raise _      
| TryWith _ 
| InDecr _ 
| EmptyList
| Cons _ 
| MatchWith _ -> []


let rec print_type = function
  | Var s -> begin match s with 
    | MNom s -> s 
    | _ -> "motif" end
  | None -> "None"
  | T c -> begin match c with
    | TInt -> "Int"
    | TBool -> "Bool"
    | TFun (arg,corps) -> "Fun : " ^ (print_type arg) ^ " -> " ^ (print_type corps)
    | TUnit -> "Unit"
    | TRef r -> "Ref : " ^ (print_type r)
    | TTuple (a,b) -> "Tuple : " ^ (print_type a) ^ " * " ^ (print_type b) 
    | TList _ -> "List" end

let rec print_prob = function
| [] -> ()
| (a,b):: l' -> print_string (print_type a);
    print_string " ----- "; 
    print_string (print_type b); print_string "\n"; print_prob l' 
;;



(*
print_prob (inf (ArithOp(Add,BConst false, Const 2)));
print_newline ();
print_prob (inf (If(CompOp (Eq, Ref (Const 3), Ref (Const 4)), ArithOp(Add,BConst false, Const 2), ArithOp(Add,Const 10, Const 2))));
print_newline ();
print_prob (inf (If(CompOp (Eq, Ref (Const 3), Ref (Const 4)), Let(false, MNom"x", Const 4, false, ArithOp(Add,Var (MNom "x"), Const 87)), ArithOp(Add,Const 10, Const 2))))
*)