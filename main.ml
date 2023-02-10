open Expr
open Affichage


(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""

let recupere_entree () =
  let showsrc = ref false 
  and debug = ref false in
  let speclist = [("-showsrc",Arg.Set showsrc,"Show source activated");("-debug",Arg.Set debug,"Debug activated")] in 
  Arg.parse speclist (fun s -> nom_fichier := s) "Bienvenue sur Fouine 1.0";
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse (), (!showsrc, !debug)
  with e -> (Printf.printf "problème de saisie\n"; raise e)

(* mettre � true et recompiler si l'on veut voir l'ex�cution pas � pas de l'automate *)
let trace = ref false
let _ = Stdlib.Parsing.set_trace !trace


      
      
(* le traitement d'une expression en entr�e *)   
let execute e options =
  let showsrc, debug = options in
  begin
    if debug then affiche_expr_tree e
    else if showsrc then (affiche_expr e; print_newline());
    if not showsrc then begin
      let v =  Expr.eval e Expr.empty_env in
      affiche_val v;
      print_newline()
    end 
  end
(* la boucle principale *)
let calc () =
  try
      let saisie, options = recupere_entree () in
	execute saisie options; flush stdout
  with e -> raise e


let _ = calc()
