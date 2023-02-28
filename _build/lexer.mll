{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | "\n\n"                                   { EOF } (* fin du fichier *)
  | [' ' '\t' '\n']                          { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | ";;"                                     { DSCOLON }
  | ";"                                      { SCOLON }
  | ","                                      { COMMA }
  | '+'                                      { PLUS }
  | '*'                                      { TIMES }
  | '-'                                      { MINUS }
  | "/"                                      { DIV }
  | "mod"                                    { MOD }
  | "()"                                     { UNIT }
  | '('                                      { LPAREN }
  | ')'                                      { RPAREN }
  | "begin"                                  { BEGIN }
  | "end"                                    { END }
  | "if"                                     { IF }
  | "then"                                   { THEN }
  | "else"                                   { ELSE }
  | "="                                      { EQ }
  | ">"                                      { G }
  | ">="                                     { GE } 
  | "<>"                                     { NE }
  | '<'                                      { L }
  | "<="                                     { LE }
  | "true"                                   { TRUE }
  | "false"                                  { FALSE }
  | "&&"                                     { AND }
  | "||"                                     { OR }
  | "not"                                    { NOT }
  | ['0'-'9']+ as s                          { INT (int_of_string s) }
  | "prInt"                                  { PRINT }
  | "let"                                    { LET }
  | "in"                                     { IN }
  | "fun"                                    { FUN }
  | "rec"                                    { REC }
  | "->"                                     { FLECHE }
  | "_"                                      { UNDERSCORE }
  | "ref"                                    { REF }
  | ":="                                     { REVAL }
  | "!"                                      { EXCL }
  | "raise"                                  { RAISE }
  | "try"                                    { TRY }
  | "with"                                   { WITH }
  | "E"                                      { E }
  | "incr"                                   { INCR }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as s { VAR (s) }
 

