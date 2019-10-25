open ASD
open Llvm
open Utils
open SymbolTable


(* main function. returns only a string: the generated code *)
let rec ir_of_ast (prog : codeObj) (symT : symbol_table)  : llvm_ir = (* TODO: change 'expression' when you extend the language *)
    (* TODO : change when you extend the language *)
    let ir, v =
        match prog with 
    |Expr(exp) ->  ir_of_expression (exp, symT) 
    |Instr (inst) -> ir_of_instruction (inst, symT)
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

and ir_of_instruction : instruction*symbol_table -> llvm_ir * llvm_value = function
    | AffectInstruction(name,e), symT -> 
            let ir0,v = ir_of_expression (e,symT) in 
            let ir = ir0 @: llvm_affect ~res_var:name ~res_type:LLVM_type_i32 ~value:v in 
            ir,LLVM_var name
            (* TODO: complete with new cases and functions when you extend your language *)

and ir_of_program (l : codeObj list) (symT : symbol_table) : llvm_ir = 
    match l with 
    | [] -> empty_ir
    | t::q -> (ir_of_ast t symT) @@ (ir_of_program q symT) 
    
and ir_of_bloc : bloc*symbol_table -> llvm_ir* llvm_value = function
    | l, symT -> 
            (ir_of_program l symT),( LLVM_i32 0)
