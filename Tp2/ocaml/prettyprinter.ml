open ASD

(* main function. return only a string *)
let rec prettyprint ast =
  match ast with
  | AddExpression (l, r) -> "(" ^ (prettyprint l) ^ " + " ^ (prettyprint r) ^ ")"
  | SubExpression (l, r) -> "(" ^ (prettyprint l) ^ " - " ^ (prettyprint r) ^ ")"
  | MulExpression (l, r) -> "(" ^ (prettyprint l) ^ " * " ^ (prettyprint r) ^ ")"
  | DivExpression (l, r) -> "(" ^ (prettyprint l) ^ " / " ^ (prettyprint r) ^ ")"
  | IntegerExpression i -> string_of_int i

(* TODO : extend when you extend the language *)
