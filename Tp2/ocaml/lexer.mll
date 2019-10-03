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
  | '+'
      { PLUS      :: tokenize lexbuf }

  (* TODO : other keywords *)

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
