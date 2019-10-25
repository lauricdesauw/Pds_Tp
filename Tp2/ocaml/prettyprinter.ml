open ASD

(* main function. return only a string *)

let rec prettyprint_expr e = 
    match e with 
    | AddExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " + " ^ (prettyprint_expr r) ^ ")"
    | SubExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " - " ^ (prettyprint_expr r) ^ ")"
    | MulExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " * " ^ (prettyprint_expr r) ^ ")"
    | DivExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " / " ^ (prettyprint_expr r) ^ ")"
    | IntegerExpression i -> string_of_int i

let rec prettyprint_instr i = 
    match i with 
    | AffectInstruction(name,e) -> name ^ (prettyprint_expr e)

let prettyprint ast =
  match ast with
  | Expr(e) ->  prettyprint_expr e
  | Instr(inst) ->  prettyprint_instr inst

(* TODO : extend when you extend the language *)
