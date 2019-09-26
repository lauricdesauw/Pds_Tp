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

(************************************************)

let blanks    = [' ' '\t' '\n']
let obj = [ 'a' - 'z' 'A' - 'Z']*


rule tokenize = parse

  (* skip new lines and update line count 
       (useful for error location) *)
  | '\n'
      { let _ = new_line lexbuf in tokenize lexbuf }

      (* skip other blanks *)
  | blanks
      { tokenize lexbuf }
  | obj 
    { STR(Lexing.lexeme lexbuf) }
  | ',' 
    { COMA }
  | ';' 
    { SEMICOLON }
  | '"' 
    { QUOT }
  | '<' 
    { L_CHEVRON }
  | '>' 
    { R_CHEVRON }

  (* catch errors *)
  | _ as c
    { let (e: error) = {
        character = c;
          line = lexbuf.lex_curr_p.pos_lnum;
          pos  = lexbuf.lex_curr_p.pos_cnum
                 - lexbuf.lex_curr_p.pos_bol
      } in
    raise (Unexpected_character e)
    }
