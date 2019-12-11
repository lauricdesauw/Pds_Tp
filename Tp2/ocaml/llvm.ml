open ASD
open Utils
   
(* TODO : extend when you extend the language *)

(* This file contains a simple LLVM IR representation *)
(* and methods to generate its string representation  *)

type llvm_type =
  | LLVM_type_i32
  | LLVM_type_i32_ptr
  | LLVM_type_tab of  int
  | LLVM_type_struct of int
  | LLVM_type_void
  | LLVM_type_string of int

(* TODO: to complete *)

type llvm_var = string

type llvm_value =
  | LLVM_i32 of int
  | LLVM_var of llvm_var



type llvm_ir = (* type of generated IR *)
  { header: llvm_instr_seq; (* instructions to be placed before all code (global definitions) *)
    body: llvm_instr_seq;
  }

 and llvm_instr_seq = (* type of sequences of instructions *)
   | Empty
   | Atom of llvm_instr
   | Concat of llvm_instr_seq * llvm_instr_seq

 and llvm_instr = string (* type of instructions *)

(* empty IR *)
let empty_ir = {
  header = Empty;
  body = Empty;
}

(* appending an instruction in the header: ir @^ i *)
let (@^) ir i = {
    header = Concat (ir.header, Atom i);
    body = ir.body;
  }

(* appending an instruction in the body: ir @: i *)
let (@:) ir i = {
    header = ir.header;
    body = Concat (ir.body, Atom i);
  }

(* concatenation of two IRs: ir1 @@ ir2 *)
let (@@) ir1 ir2 = {
    header = Concat (ir1.header, ir2.header);
    body = Concat (ir1.body, ir2.body);
}

(* actual IR generation *)
let rec string_of_type = function
  | LLVM_type_i32 -> "i32"
  | LLVM_type_i32_ptr -> "i32*"
  | LLVM_type_tab(size) -> "[ " ^ string_of_int size ^ " x i32 ]"
  | LLVM_type_void -> "void"
  | LLVM_type_string(size) -> "[ " ^ string_of_int size ^ " x i8 ]"
  | LLVM_type_struct(size) -> "[ " ^ string_of_int size ^ " x i32* ]"
and string_of_var x = x

and string_of_value = function
  | LLVM_i32 n -> string_of_int n
  | LLVM_var x -> string_of_var x

and string_of_ir ir =
  (* this header describe to LLVM the target
   * and declare the external function printf
   *)
  "; Target\n"
  ^ "target triple = \"x86_64-unknown-linux-gnu\"\n"
  ^ "; External declaration of the printf function\n"
  ^ "declare i32 @printf(i8* noalias nocapture, ...)\n"
  ^ "\n; Actual code begins\n"
  ^ string_of_instr_seq ir.header
  ^ "\n\n"
  ^ string_of_instr_seq ir.body

and string_of_instr_seq = function
  | Empty -> ""
  | Atom i -> i
  | Concat (li1,li2) -> string_of_instr_seq li1 ^ string_of_instr_seq li2

and string_of_instr i = i

let rec gen_ir_decl l_var typ id_l=
  match l_var, id_l with
  | [],_ -> empty_ir
  | symb::tab, id::id_l' ->
     match symb with
     |  Var(symb_name) -> 
         let ir0 = (gen_ir_decl  tab typ id_l') in
         (empty_ir @: "%" ^ symb_name ^ id ^ " = alloca " ^  string_of_type typ ^ "\n") @@ ir0
     | Tab(symb_name, offset_expr) -> (match offset_expr with
                                     | IntegerExpression(size) ->
                                        let ir0 = (gen_ir_decl  tab typ id_l') in
                                        (empty_ir @: "%" ^ symb_name ^id ^ " = alloca " ^  string_of_type (LLVM_type_tab(size)) ^ "\n") @@ ir0
                                     | _ -> raise Wrong_decl_expr)

let rec gen_ir_decl_fields name fields fields_type n=
  match fields,fields_type with
  | [],_ -> (empty_ir @: "%" ^ name ^ " = alloca " ^  string_of_type (LLVM_type_struct(n)) ^ "\n") 
  | t::q,t_type::q_type -> let ir0 = gen_ir_decl_fields name q q_type n in
                           (empty_ir @: "%" ^ t ^ " = alloca " ^  string_of_type (t_type) ^ "\n") @@ ir0       

let rec gen_set_struct name fields fields_type n =
  match fields, fields_type with
  | [], _ -> empty_ir
  | t::q, t_type::q_type -> let x = newtmp() in
                            let ir = gen_set_struct name q q_type (n+1) in 
                            ir @: string_of_var x ^ "= getelementptr inbounds " ^ string_of_type t_type ^ ", " ^string_of_type t_type ^ "* %" 
                              ^ string_of_var t ^ ", " ^  "i64 0, i32 " ^string_of_int n ^ "\n"
                              ^ "store " ^ string_of_type t_type ^ string_of_var x  ^ ", " ^ string_of_type t_type ^"* " ^ x  ^ "\n" 

(* functions for the creation of various instructions *)

let llvm_add ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  string_of_var res_var ^ " = add " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_sub ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  string_of_var res_var ^ " = sub nsw " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_mul ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  string_of_var res_var ^ " = mul " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_div ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  string_of_var res_var ^ " = div " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_affect ~(res_var : llvm_var) ~(res_type : llvm_type) ~(value : llvm_value) : llvm_instr =
 "store " ^ string_of_type res_type ^ " " ^ string_of_value value ^ ", " ^ string_of_type res_type ^ "* " ^ string_of_var res_var ^ "\n"

let llvm_decl ~(res_var : llvm_var) ~(res_type : llvm_type) : llvm_instr =
 "%" ^ string_of_var res_var ^ " = alloca " ^ string_of_type res_type ^ "\n"

let llvm_return ~(ret_type : llvm_type) ~(ret_value : llvm_value) : llvm_instr =
  "ret " ^ string_of_type ret_type ^ " " ^ string_of_value ret_value ^ "\n"

let rec string_of_param ~(param : llvm_var list) =
  match param with
  | [] -> ""
  | t::[] -> "i32 " ^ string_of_var t 
  | t::q -> "i32 " ^ string_of_var t ^ "," ^ string_of_param q
  
let llvm_call ~(ret_type : llvm_type) ~(fun_name : llvm_var) ~(param : llvm_var list) =
  "call " ^ string_of_type ret_type ^ " " ^ string_of_var fun_name ^ "(" ^ string_of_param param ^")\n"
  
(* defining the 'main' function with ir.body as function body *)
let llvm_define_main (ir : llvm_ir) : llvm_ir =
  { header = ir.header;
    body = Atom ("define i32 @main() {\n" ^ string_of_instr_seq ir.body ^ (llvm_return  LLVM_type_i32 (LLVM_i32 0)) ^ "}\n");
  }

let llvm_get_elem ~(st_var : llvm_var) ~(tab_type : llvm_type) ~(tab : llvm_var) ~(offset : llvm_value) : llvm_instr =
  string_of_var st_var ^ "= getelementptr inbounds " ^ string_of_type tab_type ^ ", " ^string_of_type tab_type ^ "* " ^
    string_of_var tab ^ ", " ^  "i64 0, i32 " ^ string_of_value offset^ "\n"

let rec concat_in_string l1 =
  match l1 with
  | [] -> ""
  | [t1] -> "i32 %"^ string_of_var t1 
  | t1::t2::q1 -> "i32 %"^ string_of_var t1 ^ ", " ^ (concat_in_string (t2::q1))

let llvm_if_then_else ~(ir_cond : llvm_ir) ~(ir_then : llvm_ir) ~(ir_else : llvm_ir) ~(if_value : llvm_value) ~(if_var : llvm_var) ~(id : string) =
  let cond_instr  = ir_cond @: string_of_var if_var ^ " = icmp ne i32 " ^ string_of_value if_value ^ ", 0 \nbr i1 " ^ string_of_var if_var ^ ", label %then" ^ id ^ ", label %else" ^ id ^ " \n" in 
  let then_instr = (cond_instr @: "then"  ^ id ^ ":\n") @@ ir_then @: "br label %fi" ^ id ^ "\n" in
  let else_instr = (then_instr @:  "else" ^ id ^ ":\n") @@  ir_else @: "br label %fi" ^id ^ "\n" in
  else_instr @: "fi" ^ id ^ ":\n" 

let llvm_load ~(store_var : llvm_var) ~(load_var : llvm_var) ~(load_type : llvm_type) : llvm_instr =
  string_of_var store_var ^ "= load " ^ string_of_type load_type ^ ", " ^ string_of_type load_type ^ "* " ^ string_of_var load_var ^ "\n"
  
let llvm_while  ~(ir_cond : llvm_ir) ~(ir_body : llvm_ir) ~(cond_var : llvm_var) ~(cond_value : llvm_value) ~(id : string) =
  let cond_instr = (empty_ir @:"br label %while" ^ id ^ "\n" ^ "while" ^ id ^ ":\n")
                   @@ ir_cond @: string_of_var cond_var ^ " = icmp ne i32 " ^ string_of_value cond_value ^ ", 0 \nbr i1 " ^ string_of_var cond_var
                                                                     ^ ", label %do" ^ id ^ ", label %done" ^ id ^ " \n" in 
  let do_instr = (cond_instr @: "do" ^ id ^ ":\n") @@ ir_body  @: "br label %while" ^ id ^"\n" in
  do_instr @:  "done" ^ id ^ ":\n" 

let llvm_funct ~(ret_type : llvm_type) ~( funct_name : llvm_var) ~(body_ir : llvm_ir) ~(param : llvm_var list) =
  let param_string = concat_in_string param in 
  let head = empty_ir @: ("define " ^ string_of_type ret_type ^ string_of_var funct_name ^ "(" ^ param_string ^ ") {\n") in
  let is_void = if (ret_type = LLVM_type_void) then "ret void" else "ret i32 0" in 
  let queue = empty_ir @:  is_void ^ "\n}\n" in
  head @@ body_ir @@ queue 

let rec str_of_list_print l_var =
  match l_var with
  | []-> ""
  | t_v::q_v -> ", i32 " ^ string_of_var t_v ^ str_of_list_print q_v
              
let llvm_string ~(var : llvm_var) ~(string_value : string) ~(size : int)  =
  string_of_var var ^ "= global [" ^ string_of_int size ^ " x i8] c\"" ^ string_value  ^ "\\" ^ "00" ^ "\""
                          
let llvm_print  ~(str_var : llvm_var)  ~(str_type : llvm_type) ~(l_var : llvm_var list) : llvm_instr =
  let str_v = "getelementptr inbounds (" ^ string_of_type str_type ^ ", " ^string_of_type str_type ^ "* " ^
                string_of_var str_var ^ ", " ^  "i64 0, i64 0)" in
    "call i32 (i8*, ... ) @printf(i8* " ^ str_v ^ str_of_list_print l_var ^ ")\n"
  

let llvm_read ~(str_var : llvm_var)  ~(str_type : llvm_type) ~(var_type : llvm_type) ~(l_var : llvm_var list) : llvm_instr =
let str_v = "getelementptr inbounds (" ^ string_of_type str_type ^ ", " ^string_of_type str_type ^ "* " ^
                string_of_var str_var ^ ", " ^  "i64 0, i64 0)" in
  "call i32 (i8*, ... ) @scanf(i8* " ^ str_v ^ "," ^ str_of_list_print l_var ^ ")\n"

let llvm_get_field ~(st_var : llvm_var) ~(strct_type : llvm_type) ~(strct : llvm_var) ~(offset : int ) : llvm_instr =
  string_of_var st_var ^ "= getelementptr inbounds " ^ string_of_type strct_type ^ ", " ^string_of_type strct_type ^ "* " ^
    string_of_var strct ^ ", " ^  "i64 0, i32 " ^ string_of_int offset^ "\n"







