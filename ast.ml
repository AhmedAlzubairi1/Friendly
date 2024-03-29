(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mul | Div | Equal | Neq | Less | 
          And | Or | LessEqual | Greater| GreaterEqual

type typ = Int | Bool | String | Float | Chunk of string

type expr =
    Literal of int
  | BoolLit of bool
  | FLiteral of string
  | Id of string
  | StringWord of string
  | Binop of expr * op * expr
  | Assign of expr * expr
  (* function call *)
  | Call of string * expr list
  | ChunkLit of string
  | Colon of expr * string

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;  (* friendly doesn't specify return type. It needs to so that it works  *) 
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}


type chunk_def = {

  cname: string;
  cfields: bind list;

}


type program = bind list * func_def list * chunk_def list























(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Mul -> "*"
  | Div -> "/"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
  | Or -> "||"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | FLiteral(l) -> l
  | StringWord(a) -> a
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ChunkLit(s) -> s
  | Colon(e, s) -> string_of_expr e ^ " " ^ s


let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Float -> "float"
  | Chunk(s) -> "chunk" ^ s

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_cdecl cdecl =
  cdecl.cname ^ "{\n" ^
  String.concat "" (List.map string_of_vdecl cdecl.cfields) ^
  "}\n"



let string_of_program (vars, funcs, chunks) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_cdecl chunks)
