(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions, chunks) =

  (* Verify a list of bindings has no duplicate names *)
  let check_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Make sure no globals duplicate *)
  check_binds "global" globals;




(* concept of ensuring that chunks do not have recursive definitions
 * adopted and modified from English project from Fall 2017
 http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/index.html*)



  let find_cdecl_from_cname chunk_t_name =
    try List.find (fun t-> t.cname= chunk_t_name) chunks 
      with Not_found -> raise (Failure("chunk " ^ chunk_t_name ^ "not found")) 
  in
  let rec check_rec_chunk_h cdecl chunks_known_set =
    let check_for_repetition chunk_t_name =
      if StringSet.mem chunk_t_name chunks_known_set 
      then raise (Failure ("recursive chunk definition"))
      else check_rec_chunk_h (find_cdecl_from_cname chunk_t_name)  
      (StringSet.add chunk_t_name chunks_known_set)
    in
    let chunk_field_check = function
      (Chunk c, _) -> check_for_repetition c
      | _ -> () 
    in
    List.iter (chunk_field_check) cdecl.cfields 
  in
  let check_recursive_chunk cdecl =
     check_rec_chunk_h cdecl StringSet.empty    
  in
  List.map check_recursive_chunk chunks;





(* Methods for ensuring fields accessed from chunks actually belong to that chunk
 * adopted and modified from English project from Fall 2017
 http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/index.html*)

  let resolve_chunk_access cname field =
    let  c = try List.find (fun t -> t.cname = cname) chunks
      with Not_found -> raise (Failure("chunk " ^ cname ^ " not found")) in
    try fst( List.find (fun s -> snd(s) = field) c.cfields) 
      with Not_found -> raise (Failure("Field " ^ field ^ " not found in chunk" ^ cname))
  in

  let check_access lhs rhs =
     match lhs with
       Chunk c -> resolve_chunk_access c rhs
       | _ -> raise (Failure(string_of_typ lhs^ " is not a chunk"))

  in

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name,ty)= StringMap.add name {
      rtyp = ty;
      fname = name;
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [("showNumber",Int);
                                                ("showTruth",Bool);
                                                ("showWords",String);
                                                ("showDecimal",Float)]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Function that takes a chunklit and returns type*)
    let get_chunk_typ_from_id cid =
      try StringMap.find cid symbols
        with Not_found -> raise (Failure (cid ^ " not found"))
    in


    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in


    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = 
      let check_bool_expr e =
        let (t, e') = check_expr e in
        match t with
        | Bool -> (t, e')
        |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
      in

      
      function
        Literal l -> (Int, SLiteral l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringWord l -> (String, SStringWord l)
      | FLiteral l -> (Float, SFLiteral l)
      | ChunkLit l -> (get_chunk_typ_from_id l, SChunkLit l)
      | Colon(e, l) -> 
         let checked_exp = check_expr e in
         let ty = match checked_exp with (rt, e') -> rt in
         (check_access ty l, SColon(checked_exp, l))
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        let checked_var = check_expr var in
        let lt = fst checked_var 
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(checked_var, (rt, e')))

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
            Add | Sub | Mul | Div when t1 = Int -> Int
            |Add | Sub | Mul | Div when t1 = Float -> Float
            |Add   when t1 = String -> String
            | Equal | Neq | Less | LessEqual | Greater | GreaterEqual -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
  
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))

    in

    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = check_stmt_list func.body
    }
  in

  let check_chunk chunk =
    check_binds "fields" chunk.cfields;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in

     
      
    (* Build local symbol table of variables for this chunk *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty (globals @ chunk.cfields )
    in

    (* need a function that takes a chunklit and returns type*)
    let get_chunk_typ_from_id cid =
      try StringMap.find cid symbols
        with Not_found -> raise (Failure (cid ^ " not found"))
    in


    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = 
      let check_bool_expr e =
        let (t, e') = check_expr e in
        match t with
        | Bool -> (t, e')
        |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))

      in      
      function
        Literal l -> (Int, SLiteral l)
      | BoolLit l -> (Bool, SBoolLit l)
      | StringWord l -> (String, SStringWord l)
      | FLiteral l -> (Float, SFLiteral l)
      | ChunkLit l -> (get_chunk_typ_from_id l, SChunkLit l)
      | Colon(e, l) -> 
         let checked_exp = check_expr e in
         let ty = match checked_exp with (rt, e') -> rt in
         (check_access ty l, SColon(checked_exp, l))
      | Id var -> (type_of_identifier var, SId var)
      | Assign(var, e) as ex ->
        (*let lt = type_of_identifier var*)
        let checked_var = check_expr var in
        let lt = fst checked_var
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(checked_var, (rt, e')))

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
            Add | Sub | Mul | Div when t1 = Int -> Int
            |Add | Sub | Mul | Div when t1 = Float -> Float
            |Add   when t1 = String -> String
            | Equal | Neq | Less | LessEqual | Greater | GreaterEqual -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        else raise (Failure err)
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
               let (et, e') = check_expr e in
               let err = "illegal argument found " ^ string_of_typ et ^
                         " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
               in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in


    let rec check_stmt_list =function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt =function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
    in (* body of check_chunk *)
    {
      scname = chunk.cname;
      scfields = chunk.cfields;
      scassigns = check_stmt_list chunk.cassigns
    }
  in  
  (globals, List.map check_func functions, List.map check_chunk chunks)
