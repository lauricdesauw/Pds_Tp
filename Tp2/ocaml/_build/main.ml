open Lexer

let _ =
  try
    (* lexical and syntactic analysis *)
    let lexbuf = Lexing.from_channel stdin in
    let token_stream = Stream.of_list (Lexer.tokenize lexbuf) in
    let ast = Parser.program token_stream in

    (* Activate one of these output: pretty-print or LLVM IR *)

    (* Pretty-print input *)
    (*print_endline (Prettyprinter.prettyprint ast)*)

    (* Print LLVM IR *)
    let ir = Codegen.ir_of_ast ast in
    print_endline (Llvm.string_of_ir ir)

  with
    Lexer.Unexpected_character e ->
    Printf.printf "Unexpected character: `%c' at position '%d' on line '%d'\n"
		  e.character e.pos e.line;
    exit 1


