open Expr


(* fonction d'affichage comparateur *) 
let affiche_comp = function
  | L  -> " < "
  | Le -> " <= "
  | G  -> " > "
  | Ge -> " >= "
  | Eq -> " = "
  | Ne -> " <> "

(* fonction d'affichage opérateurs arithmétiques *)
let affiche_arith = function
  | Add -> " + "
  | Min -> " - "
  | Mul -> " * "
  | Div -> " / "
  | Mod -> " mod "

(* fonction d'affichage opérateurs booléens *)
let affiche_bool = function
  | And -> " && "
  | Or  -> " || "
  | Not -> "not "


let rec affiche_expr e =
  let print_parenthese e = print_string "("; affiche_expr e ; print_string ")" in
  match e with
  | Const n            -> print_int n
  | BConst b           -> print_string (if b then "true" else "false")
  | Var x              -> print_string x
  | Unit               -> print_string "()"
  | ArithOp (op,e1,e2) -> (match e1 with
            | If _ -> print_parenthese
            | ArithOp (op',_,_) when op' = Add || op' = Min -> (match op with
                      | Div
                      | Mul -> print_parenthese
                      | _   -> affiche_expr)
            | _ -> affiche_expr) e1;
          print_string (affiche_arith op); (match e2 with
            | ArithOp (op',_,_) when op' = Add || op' = Min -> (match op with
                      | Div
                      | Mul -> print_parenthese
                      | _   -> affiche_expr)
            | _ -> affiche_expr) e2

  | CompOp (op,e1,e2) -> (match e1 with
            | BoolOp _ 
            | If _ -> print_parenthese
            | _ -> affiche_expr) e1;
            print_string (affiche_comp op);
            (match e2 with
            | BoolOp _ -> print_parenthese
            | _ -> affiche_expr) e2

  | BoolOp (op,e1,e2) -> begin match op with
            | Not -> print_string " not "; (match e1 with
                      | BConst _ -> affiche_expr
                      | _ -> print_parenthese) e1
            | And -> (match e1 with
                      | BoolOp (op',_,_) when op' = Or || op' = And -> print_parenthese
                      | If _ -> print_parenthese
                      | _ -> affiche_expr) e1;
                    print_string " && ";
                    (match e2 with
                      | BoolOp (op',_,_) when op' = Or -> print_parenthese
                      | _ -> affiche_expr) e2
            | Or -> (match e1 with
                      | If _ -> print_parenthese
                      | _ -> affiche_expr) e1;
                    print_string " || ";
                    affiche_expr e2
            end
  | If (c1,e1,e2) -> print_string "if " ; affiche_expr c1; print_string " then " ;affiche_expr e1; print_string " else "; affiche_expr e2
  | PrInt (e1)    -> print_string "prInt "; (match e1 with
            | Const _ -> affiche_expr
            | _ -> print_parenthese) e1  
  | Let (x,e1,e2) -> print_string "let "; print_string x; print_string " = "; affiche_expr e1; print_string " in "; affiche_expr e2
  | Fun (arg,e1)  -> print_string "fun "; print_string arg; print_string" -> "; affiche_expr e1 
  | App (e1,e2)   -> affiche_expr e1; print_string " "; (match e2 with
            | Const _
            | BConst _
            | Var _ -> affiche_expr
            | _ -> print_parenthese) e2
  | Seq (e1,e2)   -> affiche_expr e1; print_string ";\n"; affiche_expr e2


(* let affiche_val v =  print_string "TODO" *)
let affiche_val v = match v with
  | VInt k          -> print_int k
  | VBool b         -> print_string (if b then "true" else "false")
  | VFun (arg,e1,_) -> print_string "fun "; print_string arg; print_string" -> "; affiche_expr e1 
  | VUnit           -> print_string "()";
                      
