type token =
    IDENT of string
  | TEXT of string
  | INTEGER of int
  | LP
  | RP
  | LB
  | RB
  | LSQ
  | RSQ
  | COM
  | PLUS
  | MINUS
  | MUL
  | DIV
  | ASSIGN
  | FUNC_KW
  | PROTO_KW
  | INT_KW
  | VOID_KW
  | RETURN_KW
  | PRINT_KW
  | READ_KW
  | IF_KW
  | THEN_KW
  | ELSE_KW
  | FI_KW
  | WHILE_KW
  | DO_KW
  | DONE_KW
  | STRUCT_KW
  | DOT
