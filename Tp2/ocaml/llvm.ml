(* TODO : extend when you extend the language *)

(* This file contains a simple LLVM IR representation *)
(* and methods to generate its string representation  *)

type llvm_type =
  | LLVM_type_i32

(* TODO: to complete *)

type llvm_var = string

type llvm_value =
  | LLVM_i32 of int
  | LLVM_var of llvm_var
(* TODO: to complete? *)


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

let rec gen_ir_decl l_var typ =
  match l_var with
  | [] -> empty_ir
  | symb::tab ->
     let ir0 = (gen_ir_decl  tab typ) in
     ir0 @: "%" ^ symb ^ " = alloca " ^  string_of_type typ ^ "\n"          
    
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
 "store " ^ string_of_type res_type ^ " " ^ string_of_value value ^ ", " ^ string_of_type res_type ^ "* %" ^ string_of_var res_var ^ "\n"

let llvm_decl ~(res_var : llvm_var) ~(res_type : llvm_type) : llvm_instr =
 "%" ^ string_of_var res_var ^ " = alloca " ^ string_of_type res_type ^ "\n"

let llvm_return ~(ret_type : llvm_type) ~(ret_value : llvm_value) : llvm_instr =
  "ret " ^ string_of_type ret_type ^ " " ^ string_of_value ret_value ^ "\n"
(* defining the 'main' function with ir.body as function body *)
let llvm_define_main (ir : llvm_ir) : llvm_ir =
  { header = ir.header;
    body = Atom ("define i32 @main() {\n" ^ string_of_instr_seq ir.body ^ (llvm_return  LLVM_type_i32 (LLVM_i32 0)) ^ "}\n");
  }

let llvm_if_then_else ~(ir_cond : llvm_ir) ~(ir_then : llvm_ir) ~(ir_else : llvm_ir) ~(if_value : llvm_value) ~(id : string) =
  let cond_instr  = ir_cond @: "br i1 " ^ string_of_value if_value ^ ", label %then" ^ id ^ ", label %else" ^ id ^ " \n" in 
  let then_instr = (cond_instr @: "then"  ^ id ^ " : \n") @@ ir_then @: "br label %fi" ^ id ^ "\n" in
  let else_instr = (then_instr @:  "else" ^ id ^ " : \n") @@  ir_else @: "br label %fi" ^id ^ "\n" in
  else_instr @: "fi" ^ id ^ " :\n" 

let llvm_while  ~(ir_cond : llvm_ir) ~(ir_body : llvm_ir) ~(cond_value : llvm_value) ~(id : string) =
  let cond_instr = (empty_ir @: "while" ^ id ^ " :\n") @@ ir_cond @: "br i1 " ^ string_of_value cond_value
                                                                     ^ ", label %do" ^ id ^ ", label %done" ^ id ^ " \n" in 
  let do_instr = (cond_instr @: "do" ^ id ^ " :\n") @@ ir_body  @: "br label %while" ^ id ^"\n" in
  do_instr @:  "done" ^ id ^ "\n" 


let llvm_get_elem ~(st_var : llvm_var) ~(tab_type : llvm_type) ~(tab : llvm_var) ~(offset : llvm_value) : llvm_instr =
  string_of_var st_var ^ "= getelementptr inbounds " ^ string_of_type tab_type ^ ", " ^string_of_type tab_type ^ "* " ^
    string_of_var tab ^ ", " ^  "i64 0, i64 0" 



















