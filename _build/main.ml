open Expr
open Types
open Affichage
open Options

(* "incantations" qu'il n'est pas n�cessaire de comprendre dans un premier
   temps : on r�cup�re l'entr�e, dans un fichier ou sur le clavier *)
let nom_fichier = ref ""


let recupere_entree () =
  let speclist = [("-showsrc",Arg.Set Options.showsrc,"Print an ocaml code of the input");("-debug",Arg.Set Options.debug,"Debug activated");
                  ("-slow",Arg.Set Options.slow,"Eval function works step by step");("-tree",Arg.Set Options.tree,"Tree activated");
                  ("-trace",Arg.Set Options.trace,"Trace activated");("-warnings",Arg.Set Options.warnings,"Warnings activated")] in 
  Arg.parse speclist (fun s -> nom_fichier := s) "Bienvenue sur Fouine 1.0";
  let _ = Stdlib.Parsing.set_trace !Options.trace in
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
    if !Options.tree || !Options.debug then (affiche_expr_tree e; print_newline ());
    if !Options.showsrc || !Options.debug then (affiche_expr e; print_string ";;\n");
    if not !Options.showsrc then begin
      let _ =  Expr.eval e Types.empty_env in ()
    end
  end
(* la boucle principale *)
let calc () =
  try
      let saisie = recupere_entree () in
	execute saisie; flush stdout
  with e -> raise e


let _ = calc()
