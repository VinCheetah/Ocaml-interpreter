{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | '\n'            { EOL }
  | '+'             { PLUS }
  | '*'             { TIMES }
  | '-'             { MINUS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | '<'            { L }
  | "<="            { LE }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | "prInt"         { PRINT }
  | "let"          { LET }
  | "in"            { IN }
  | ['a' - 'z']+ as s { VAR(s) }
  | "="             { EQ }
  | ">"             { G }
  | ">="            { GE }
  | "<>"            { NE}
  | "&&"            { AND }
  | "||"            { OR }

  | eof             { raise Eof } (* fin du fichier *)
