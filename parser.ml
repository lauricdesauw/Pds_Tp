open ASD
open Token

let parse_cpl = parser
              | [< 'L_CHEVRON ; 'STR(x) ; 'R_CHEVRON >] -> Obj x
              | [< 'QUOT ; 'STR (x) ; 'QUOT >] -> Txt x;;

let rec parse_cpl_list = parser
                       | [< 'COMMA ; head = parse_cpl ; tail = parse_cpl_list >] -> head::tail
                       | [< >] -> []
                       | [< EOF >] -> [];;

let get_name = function
  | Obj x -> x
  | Txt x -> x;;

let parse_attr = parser
               | [< name = parse_cpl ; head = parse_cpl ; tail = parse_cpl_list >] -> Attr (get_name name, head::tail);;

let rec parse_attr_list = parser
                        | [< 'SEMICOLON ; head = parse_attr ; tail = parse_attr_list >] -> head::tail
                        | [< >] -> []
                        | [< EOF >] -> [];;

let parse_ens = parser
              | [< name = parse_cpl ; first_attr = parse_attr ; other_attr = parse_attr_list >]
                -> Ens (get_name name, first_attr::other_attr);;

let rec parse_ens_list = parser
                       | [< head = parse_ens ; 'POINT ; tail = parse_ens_list >]
                         -> head::tail
                       | [< >] -> []
                       | [< EOF >] -> [];;

let parse (tokens:token Stream.t) = Doc (parse_ens_list tokens);; 
