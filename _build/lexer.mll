{
  open Parser
exception Eof
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
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
  | "E"                                      { E }
  | "[]"                                     { EMPTYLIST }
  | "::"                                     { CONS }
  | "["                                      { LLIST }
  | "]"                                      { RLIST }
  | "try"                                    { TRY }
  | "match"                                  { MATCH }
  | "with"                                   { WITH }
  | "|"                                      { PIPE }
  | "prInt"                                  { PRINT }
  | "incr"                                   { INCR }
  | "decr"                                   { DECR }
  | "function"                               { FUNCTION }
(*| "fst"                                    { FST }
  | "snd"                                    { SND }*)
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' ''' '_']* as s { VAR (s) }
  | eof                                      { EOF } (* fin du fichier *)
 

