(* Ocamllex scanner for Friendly*)

{ open Friendlyparse }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ':'      { COLON } (*friendly*)
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
| "outputting" {OUTPUT}
(* | "number" { NUM }                *)
| "function" { FUNC }

(* RETURN *)
| "return" { RETURN }
| "number"    { INT } (*                      *)
| "int "   {INT}
| "bool"   { BOOL }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
