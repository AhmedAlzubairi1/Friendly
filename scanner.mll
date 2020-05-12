(* Ocamllex scanner for Friendly*)

{ open Microcparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let punc= [' ' '\t']
let stringWord= '"' (letter | digit | punc)* '"'
rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '-'? digit+ '.' digit+ as a {FLIT(a)}
|"decimal"   {FLOAT}
| '.'      { PERIOD } (*friendly*)
(* COMMA *)
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIV }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| ">"      { GT }
| ">="     { GTE}
| "<="     { LTE }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
(*friendly keywords*)

| "make_a" { MAKEA }
| "make"   { MAKE }
| "using"   { USING }
| "named"  { NAMED }
| "has"    { HAS }
| "be"     { BE }
| "does"   { DOES }
| "do"     { DO }
| "called" { CALLED } (* THIS IS FOR RETURN TYPE *)
(*friendly types*)
| "outputting" { OUTPUT }
(* | "number" { NUM }                *)
| "function" { FUNC }
| "chunk"  { CHUNK }
| ":"      { COLON }

(* RETURN *)
| "return" { RETURN }
| "number"    { INT } (*                      *)
| "int "   {INT}
|"sentence" {STRING}
(* | stringWord as a {SLIT(a)} *)
| '"'      {SLIT(stringCreate (Buffer.create 100) lexbuf) }
| "truth"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '-'? digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and stringCreate buf = parse
| [^'"']+ { Buffer.add_string buf  ( Lexing.lexeme lexbuf ); stringCreate buf lexbuf }
| '"'     { Buffer.contents buf}
and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

(* Source for help on string: https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53 *)
