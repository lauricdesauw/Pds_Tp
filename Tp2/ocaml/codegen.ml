open ASD
open Llvm
open Utils
open SymbolTable
open List

(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : codeObj) (symT : symbol_table)  : llvm_ir = (* TODO: change 'expression' when you extend the language *)
    (* TODO : change when you extend the language *)
    let ir, v =
        match prog with 
    |Expr(exp) ->  ir_of_expression (exp, symT) 
    |Instr (inst) -> let tmp_ir, tmp_v, _ = (ir_of_instruction (inst, symT)) in tmp_ir,tmp_v
    |Bloc(c) -> ir_of_bloc(c,symT) 
    in 
    (* adds the return instruction *)
    let ir = ir @: llvm_return ~ret_type:LLVM_type_i32 ~ret_value:v in
    (* We create the function main *)
    let ir = llvm_define_main ir in
    ir

    (* translation from VSL+ types to LLVM types *)
and llvm_type_of_asd_typ : typ -> llvm_type = function
    | Type_Int -> LLVM_type_i32

    (* all expressions have type LLVM_type_i32 *)
    (* they return code (llvm_ir) and expression result (llvm_value) *)
and ir_of_expression : expression * symbol_table -> llvm_ir * llvm_value = function
    | IntegerExpression i, symT ->
            empty_ir, LLVM_i32 i
    | VarExpression(name), symT ->
       if  lookup  symT name == None
       then empty_ir,LLVM_var name
       else (raise Undeclared_variable) 
    
    | AddExpression (e1,e2), symT ->
            let ir1, v1 = ir_of_expression (e1,symT) in
            let ir2, v2 = ir_of_expression (e2, symT) in
            let x = newtmp () in
            let ir = ir1 @@ ir2 @: llvm_add ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
            ir, LLVM_var x
    | SubExpression (e1,e2), symT->
            let ir1, v1 = ir_of_expression (e1,symT) in
            let ir2, v2 = ir_of_expression (e2, symT) in
            let x = newtmp () in
            let ir = ir1 @@ ir2 @: llvm_sub ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
            ir, LLVM_var x
    | MulExpression (e1,e2) , symT->
            let ir1, v1 = ir_of_expression (e1,symT) in
            let ir2, v2 = ir_of_expression (e2, symT) in
            let x = newtmp () in
            let ir = ir1 @@ ir2 @: llvm_mul ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
            ir, LLVM_var x
    | DivExpression (e1,e2) , symT ->
            let ir1, v1 = ir_of_expression (e1, symT) in
            let ir2, v2 = ir_of_expression (e2, symT) in
            let x = newtmp () in
            let ir = ir1 @@ ir2 @: llvm_div ~res_var:x ~res_type:LLVM_type_i32 ~left:v1 ~right:v2 in
            ir, LLVM_var x

and ir_of_instruction : instruction * symbol_table -> llvm_ir * llvm_value * symbol_table = function
    | AffectInstruction(name,e), symT -> 
       let ir0,v = ir_of_expression (e,symT) in 
       let ir = ir0 @: llvm_affect ~res_var:name ~res_type:LLVM_type_i32 ~value:v in 
       ir,(LLVM_var name),symT
       
    
    | DeclInstruction(typ,l_var), symT -> (gen_ir_decl l_var (llvm_type_of_asd_typ typ)), (LLVM_i32 0), (add_list typ l_var symT)

    | IfElseInstruction(cond,then_bloc,else_bloc), symT ->
       let ir_if,v_if = ir_of_expression (cond,symT) in
       let ir_then,v_then = ir_of_bloc (then_bloc, symT) in
       let ir_else,v_else = ir_of_bloc (else_bloc, symT) in
       let id = new_labels_id () in 
       let ir = llvm_if_then_else ~ir_cond:ir_if ~ir_then:ir_then ~ir_else:ir_else ~if_value:v_if ~id:id in
       ir, (LLVM_i32 0),symT
       
and ir_of_program (l : codeObj list) (symT : symbol_table) : llvm_ir = 
    match l with 
    | [] -> empty_ir
    | t::q -> (ir_of_ast t symT) @@ (ir_of_program q symT) 

and map_aux symT instr = instr,symT

and proj_first_elem tripl =
  match tripl with
  | ir, _, _  -> ir 

and proj_third_elem tripl =
  match tripl with
  | _, _, sym  -> sym

and ir_of_bloc : bloc * symbol_table -> llvm_ir * llvm_value = function
  | (instr_l,codeObj_list ), symT ->
     let instr_list = List.map (map_aux []) instr_l  in
     let tmp_list = List.map (ir_of_instruction) instr_list in

     let ir_list = List.map (proj_first_elem) tmp_list in 
     let ir0 = List.fold_left (@@) empty_ir ir_list in

     let tmp_sym_list = List.map (proj_third_elem) tmp_list in
     let sym0 = (List.fold_left (@) [] tmp_sym_list) @  symT in 

     (ir0 @@ (ir_of_program codeObj_list (sym0) ) ),( LLVM_i32 0)






































