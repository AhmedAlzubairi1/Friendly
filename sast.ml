(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SFLiteral of string
  | SStringWord of string
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SAssign of sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SChunkLit of string
  | SColon of sexpr * string


type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type schunk_def = {
  
  scname: string;
  scfields: bind list;

  scassigns: sstmt list;

} 

type sprogram = bind list * sfunc_def list * schunk_def list



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SId(s) -> s
      | SFLiteral(s) -> s
      | SStringWord(a) -> a
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
      | SChunkLit(s) -> s 
      | SColon(e, s) -> string_of_sexpr e ^ " " ^ s
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"


let string_of_scdecl cdecl = 

  cdecl.scname ^ "{\n" ^
  String.concat "" (List.map string_of_vdecl cdecl.scfields) ^
  String.concat "" (List.map string_of_sstmt cdecl.scassigns) ^
  "}\n"



let string_of_sprogram (vars, funcs, chunks) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_scdecl chunks)
