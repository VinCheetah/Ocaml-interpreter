open Expr
open Affichage

(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""


let recupere_entree () =
  let speclist = [("-showsrc",Arg.Set Expr.(Options.showsrc),"Print an ocaml code of the input");("-debug",Arg.Set Expr.(Options.debug),"Debug activated");
                  ("-slow",Arg.Set Expr.(Options.slow),"Eval function works step by step");("-tree",Arg.Set Expr.(Options.tree),"Tree activated");
                  ("-trace",Arg.Set Expr.(Options.trace),"Trace activated")] in 
  Arg.parse speclist (fun s -> nom_fichier := s) "Bienvenue sur Fouine 1.0";
  let _ = Stdlib.Parsing.set_trace !Expr.(Options.trace) in
  try
    let where_from = match !nom_fichier with
      | "" -> stdin
      | s -> open_in s in
    let lexbuf = Lexing.from_channel where_from in
    let parse () = Parser.main Lexer.token lexbuf in
    parse ()
  with e -> (Printf.printf "problème de saisie\n"; raise e)

(* mettre � true et recompiler si l'on veut voir l'ex�cution pas � pas de l'automate 
let trace = ref true
let _ = Stdlib.Parsing.set_trace !trace
*)

      
      
(* le traitement d'une expression en entr�e *)   
let execute e =
  begin
    if !Expr.(Options.tree) || !Expr.(Options.debug) then (affiche_expr_tree e; print_newline ());
    if !Expr.(Options.showsrc) || !Expr.(Options.debug) then (affiche_expr e; print_newline());
    if not !Expr.(Options.showsrc) then begin
      let v =  Expr.eval e Expr.empty_env in
      print_string "\nout : ";affiche_val v;
      print_newline()
    end 
  end
(* la boucle principale *)
let calc () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e


let _ = calc()
