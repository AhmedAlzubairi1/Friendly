(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)



(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions, chunks) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and str_t      = L.pointer_type (L.i8_type context)
  and i1_t       = L.i1_type     context 
  and float_t    = L.double_type context   in




(*concept of using a hashtable to map chunk name to lltype
  adopted and modified from English project from Fall 2017
 http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/index.html*)




let chunk_name_to_type:(string, L.lltype) Hashtbl.t = Hashtbl.create 5
in 

let fill_chunk_type_table cdecl =
  let chunk_type = L.named_struct_type context cdecl.scname in
  Hashtbl.add chunk_name_to_type cdecl.scname chunk_type in 
  List.map fill_chunk_type_table chunks; 

let get_chunk_type cname = try Hashtbl.find chunk_name_to_type cname
  with Not_found -> raise(Failure(cname ^ " not found"))
in 


  (* Return the LLVM type for a Friendly type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.String -> str_t
    | A.Float  -> float_t
    | A.Chunk(cname) -> get_chunk_type cname
  in


(* Define chunks and fill hashtable *)
let make_chunk_body cdecl =
  let chunk_typ = try Hashtbl.find chunk_name_to_type cdecl.scname
    with Not_found -> raise(Failure(cdecl.scname ^ " not defined")) in
  let cfield_types = List.map (fun (t, n) -> t) cdecl.scfields in
  let cfield_lltypes = Array.of_list (List.map ltype_of_typ cfield_types) in
  L.struct_set_body chunk_typ cfield_lltypes true in
  List.map make_chunk_body chunks;


let chunk_field_indices =
  let handles m chnk = 
    let field_names = List.map (fun (t, n) -> n) chnk.scfields in
    let add_one n = n + 1 in
    let add_fieldindex (m, i) field_name =
      (StringMap.add field_name (add_one i) m, add_one i) in
    let chunk_field_map = 
      List.fold_left add_fieldindex (StringMap.empty, -1) field_names
    in
    StringMap.add chnk.scname (fst chunk_field_map) m  
  in
  List.fold_left handles StringMap.empty chunks
  in





  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with 
      A.Float -> L.const_float (ltype_of_typ t) 0.0
    | _ -> L.const_int (ltype_of_typ t) 0

      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
(* for no returns, do void type. str_t for malloc  input *)

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in
(* var_arg part means variable amount of inputs for function *)
(* i32_t is the ouputput, the array stuff is the variable input. Thier types *)

  let strcpy_t : L.lltype =
          L.function_type str_t [| str_t; str_t |] in

  let strcpy_func : L.llvalue =
    L.declare_function "strcpy" strcpy_t the_module in
  
  let strcat_t: L.lltype=
    L.function_type str_t [| str_t; str_t |] in
  let strcat_func: L.llvalue =
    L.declare_function "strcat" strcat_t the_module in 
  let strlen_t: L.lltype=
    L.function_type i32_t [| str_t  |] in
  let strlen_func: L.llvalue =
    L.declare_function "strlen" strlen_t the_module in 
  
  let malloc_t: L.lltype=
          L.function_type str_t [| i32_t |] in
  let malloc_func: L.llvalue=
          L.declare_function "malloc" malloc_t the_module in 

  let strcmp_t: L.lltype =
          L.function_type i32_t [| str_t; str_t |] in
  let strcmp_func: L.llvalue= L.declare_function "strcmp" strcmp_t the_module 
  in   



(*  strcpy is name of the c function *)
(*  printf_t is lltype printf_func*)
(* declare_function generates function declaration. It is the header file. the_module represents the program as a whole while builder is the current location *)

   (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder 
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n"  "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
             in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let stringCompare a b c d=
            let zero= L.const_int i32_t 0 in 

            let t = L.build_call strcmp_func [| a; b |] "tmp" d in 
            let truthValue = L.build_icmp L.Icmp.Eq t zero "tmo" d in
            truthValue
            in

    let stringCompareNeg a b c d=
            let zero= L.const_int i32_t 0 in 

            let t = L.build_call strcmp_func [| a; b |] "tmp" d in 
            let truthValue = L.build_icmp L.Icmp.Ne t zero "tmo" d in
            truthValue
            in



    let lessString a b c d =
            let zero= L.const_int i32_t 0 in 

            let t = L.build_call strcmp_func [| a; b |] "tmp" d in
            let truthValue= L.build_icmp L.Icmp.Slt t zero "tmp" d in
            truthValue
            in 

    let greaterString a b c d =
            let zero= L.const_int i32_t 0 in 

            let t = L.build_call strcmp_func [| a; b |] "tmp" d in
            let truthValue= L.build_icmp L.Icmp.Sgt t zero "tmp" d in
            truthValue
            in 

    let greaterEqualString a b c d =
            let zero= L.const_int i32_t 0 in 

            let t = L.build_call strcmp_func [| a; b |] "tmp" d in
            let truthValue= L.build_icmp L.Icmp.Sge t zero "tmp" d in
            truthValue
            in 
    let lessEqualString a b c d =
            let zero= L.const_int i32_t 0 in 

            let t = L.build_call strcmp_func [| a; b |] "tmp" d in
            let truthValue= L.build_icmp L.Icmp.Sle t zero "tmp" d in
            truthValue
            in 


    let newFunction a b c d =
       
       (* char * tmp; *)
       let size1 = L.build_call strlen_func [| a |] "tmp" d in 
       let size2 = L.build_call strlen_func [| b |] "tmp" d in
       let one= L.const_int i32_t 1 in (* THis is supposed to add 1 *)

       let sizeAddOne=L.build_add size1 one "tmp" d in 
       let totalSize= L.build_add sizeAddOne size2 "tmp" d in (* remember to add one to totalSize *)
       let newString= L.build_call malloc_func [| totalSize|] "tmp" d in 
       let cpyOne= L.build_call strcpy_func  [| newString; a |] "tmp" d in 
       let finalCopy= L.build_call strcat_func [|newString; b |] "tmp" d in 
         finalCopy
         
    in


    (*addr_of_expr modified from English project from Fall 2017
     http://www.cs.columbia.edu/~sedwards/classes/2017/4115-fall/index.html*)

    let addr_of_expr expr builder = 
     match expr with
       SId s -> (lookup s)
     | SChunkLit c -> (lookup c)
     | SColon (e1, field) ->
         let sx = match e1 with (se, sx) -> sx in
          (match sx with
         SId s ->
           let etype = fst( 
           try List.find (fun n -> snd(n) = s) fdecl.slocals
             with Not_found -> try List.find (fun n -> snd(n) = s) fdecl.sformals
               with Not_found -> raise (Failure("Unable to find" ^ s )))

           in
           (try match etype with
             Chunk t->
               let index_number_list = StringMap.find t chunk_field_indices in
               let index_number = StringMap.find field index_number_list in
               let chunk_llvalue = lookup s in
               let access_llvalue = L.build_struct_gep chunk_llvalue index_number "tmp" builder in
               access_llvalue
           | _ -> raise (Failure("not found"))
          with Not_found -> raise (Failure("not found" ^ s)))
          | _ -> raise (Failure("lhs not found")))
      | _ -> raise (Failure("addr not found"))

  in
  (* returns final llvalue of pointer with adding 
     Should pass builder on to anything if needed *)
    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFLiteral l -> L.const_float_of_string float_t l
      | SStringWord a -> L.build_global_stringptr a "temp" builder
      | SId s       -> L.build_load (lookup s) s builder
      | SChunkLit c -> lookup c
      | SColon (e, field) ->
        let sx = match e with (se, sx) -> sx in
        let llvalue = (addr_of_expr sx builder) in
        let built_e = build_expr builder e in
        let built_e_lltype = L.type_of built_e in
        let built_e_opt = L.struct_name built_e_lltype in
        let built_e_name = (match built_e_opt with
                             | None -> ""
                             | Some(s) -> s)
        in
        let indices = StringMap.find built_e_name chunk_field_indices in
        let index = StringMap.find field indices in
        let access_llvalue = L.build_struct_gep llvalue index "tmp" builder in
                             L.build_load access_llvalue "tmp" builder

      | SAssign (e1, e2) ->
         let e1_sx = match e1 with (se, sx) -> sx in
         let l_val = (addr_of_expr e1_sx builder) in
         let e2' = build_expr builder e2 in
        ignore(L.build_store e2' l_val builder); e2'
      | SBinop((A.String,_) as e1,op,e2) ->
         let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> newFunction
         | A.Equal   -> stringCompare
         | A.Neq     -> stringCompareNeg
         | A.Less    -> lessString
         | A.LessEqual -> lessEqualString
         | A.Greater -> greaterString
         | A.GreaterEqual -> greaterEqualString       
        ) e1' e2' "tmp" builder 
        (* Need to change right side *)
     
        | SBinop ((A.Float,_ ) as e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_fadd
         | A.Sub     -> L.build_fsub
         | A.Mul     -> L.build_fmul
         | A.Div     -> L.build_fdiv
         | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
         | A.Neq     -> L.build_fcmp L.Fcmp.One
         | A.Less    -> L.build_fcmp L.Fcmp.Olt
         | A.LessEqual -> L.build_fcmp L.Fcmp.Ole
         | A.Greater -> L.build_fcmp L.Fcmp.Ogt
         | A.GreaterEqual -> L.build_fcmp L.Fcmp.Oge
        ) e1' e2' "tmp" builder
         | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.Mul     -> L.build_mul
         | A.Div     -> L.build_sdiv
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
         | A.LessEqual -> L.build_icmp L.Icmp.Sle
         | A.Greater -> L.build_icmp L.Icmp.Sgt
         | A.GreaterEqual -> L.build_icmp L.Icmp.Sge
         ) e1' e2' "tmp" builder
      | SCall ("showNumber", [e]) | SCall("showTruth",[e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall("showWords",[e]) ->
     L.build_call printf_func [| string_format_str ; (build_expr builder e) |]
     "printf" builder      
     | SCall("showDecimal",[e]) ->
     L.build_call printf_func [| float_format_str ; (build_expr builder e) |]
     "printf" builder  
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module
