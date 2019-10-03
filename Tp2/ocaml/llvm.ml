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

			  
(* functions for the creation of various instructions *)

let llvm_add ~(res_var : llvm_var) ~(res_type : llvm_type) ~(left : llvm_value) ~(right : llvm_value) : llvm_instr =
  string_of_var res_var ^ " = add " ^ string_of_type res_type ^ " " ^ string_of_value left ^ ", " ^ string_of_value right ^ "\n"

let llvm_return ~(ret_type : llvm_type) ~(ret_value : llvm_value) : llvm_instr =
  "ret " ^ string_of_type ret_type ^ " " ^ string_of_value ret_value ^ "\n"

(* defining the 'main' function with ir.body as function body *)
let llvm_define_main (ir : llvm_ir) : llvm_ir =
  { header = ir.header;
    body = Atom ("define i32 @main() {\n" ^ string_of_instr_seq ir.body ^ "}\n");
  }
									 
(* TODO: complete with other LLVM instructions *)
								
