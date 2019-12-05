{
  open Lexing
  open Token

  type error = {
    character: char;
    line: int;
    pos: int;
  }

  exception Unexpected_character of error
}

(**********************************************************)

let letter = ['a'-'z']
let digit  = ['0'-'9']
let ascii  = _ # ['\n' '"']
let blanks = [' ' '\n' '\t']

rule tokenize = parse
  (* skip new lines and update line count (useful for error location) *)
  | '\n'
      { let _ = new_line lexbuf in tokenize lexbuf }

  (* skip other blanks *)
  | blanks
      { tokenize lexbuf }

  (* skip comments *)
  | "//" (_ # '\n')*
      { tokenize lexbuf }

  (* characters *)
  | '('
      { LP        :: tokenize lexbuf }
  | ')'
      { RP        :: tokenize lexbuf }
  | '{'
      { LB        :: tokenize lexbuf }
  | '}'
      { RB        :: tokenize lexbuf }
  | ":="
      { ASSIGN    :: tokenize lexbuf }
  | '+'
      { PLUS      :: tokenize lexbuf }
  | '-'
      { MINUS     :: tokenize lexbuf }
  | '*'
      { MUL       :: tokenize lexbuf }
  | '/'
      { DIV       :: tokenize lexbuf }
  | "IF"
      { IF_KW       :: tokenize lexbuf }
  | "THEN"
      { THEN_KW       :: tokenize lexbuf }
  | "ELSE"
      { ELSE_KW       :: tokenize lexbuf }
  | "FI"
      { FI_KW::tokenize lexbuf }
  | "WHILE"
      { WHILE_KW::tokenize lexbuf }
  | "DO"
      { DO_KW::tokenize lexbuf }
  | "DONE"
      { DONE_KW::tokenize lexbuf }
  | "INT"
      { INT_KW::tokenize lexbuf }
  | ","
      { COM::tokenize lexbuf }
  | "["
      { LSQ::tokenize lexbuf }
  | "]"
      { RSQ::tokenize lexbuf }
  | "PROTO"
      { PROTO_KW::tokenize lexbuf }
  | "FUNC"
      { FUNC_KW::tokenize lexbuf }
  | "RETURN"
      { RETURN_KW::tokenize lexbuf }
  | "PRINT"
      { PRINT_KW::tokenize lexbuf }
  | "READ"
      { READ_KW::tokenize lexbuf }

  (* other tokens (no conflict with keywords in VSL) *)
  | letter (letter | digit)* as lxm
      { IDENT lxm :: tokenize lexbuf }
  | '"' (ascii* as lxm) '"'
      { TEXT lxm  :: tokenize lexbuf }
  | (digit+) as lxm
      { INTEGER (int_of_string lxm) :: tokenize lexbuf }

  (* end-of-file : end up with the empty stream *)
  | eof
      { [] }

  (* catch errors *)
  | _ as c
    {
      let e = {
          character = c;
          line = lexbuf.lex_curr_p.pos_lnum;
          pos  = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol;
        }
      in raise (Unexpected_character e)
    }
