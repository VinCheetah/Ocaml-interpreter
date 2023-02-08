open Expr




(* fonction d'affichage opérateur*) 

let affiche_op op = match op with
  |L -> "<"
  |Le -> "<="
  |G -> ">"
  |Ge -> ">="
  |Eq -> "="
  |Ne -> "<>"
  |And -> "&&"
  |Or -> "||"
  | _ -> failwith "pas encore traité"
(* fonction d'affichage *)
let rec affiche_expr e =
  let aff_aux a s b = 
      begin
  print_string "(";
  affiche_expr a;
	print_string s;
	(*print_string ", ";*)
	affiche_expr b;
	print_string ")"
      end
  in
  match e with
  | Var k -> print_string k
  | Const k -> print_int k
  | Add(e1,e2) -> aff_aux e1 "+" e2
  | Mul(e1,e2) -> aff_aux  e1 "*" e2
  | Min(e1,e2) -> aff_aux e1 "-" e2
  | Op(op,e1,e2) -> let s  = (affiche_op op) in aff_aux e1 s e2
  | If(c1,e1,e2) -> print_string "if " ; affiche_expr c1; print_string " then " ;affiche_expr e1; print_string " else "; affiche_expr e2
  | PrInt(e1) -> print_string "prInt ( "; affiche_expr e1 ; print_string ")"  
  | Let(x,e1,e2) -> print_string "let "; print_string x; print_string "="; affiche_expr e1; print_string "\n in "; affiche_expr e2
(* let affiche_val v =  print_string "TODO" *)
let affiche_val v = match v with
  |VInt k -> print_int k
                      
