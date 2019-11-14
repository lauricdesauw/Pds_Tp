open ASD
open List
    
(* main function. return only a string *)

let rec prettyprint_expr e =
    match e with
    | AddExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " + " ^ (prettyprint_expr r) ^ ")"
    | SubExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " - " ^ (prettyprint_expr r) ^ ")"
    | MulExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " * " ^ (prettyprint_expr r) ^ ")"
    | DivExpression (l, r) -> "(" ^ (prettyprint_expr l) ^ " / " ^ (prettyprint_expr r) ^ ")"
    | IntegerExpression i -> string_of_int i

and prettyprint_instr i =
    match i with
    | AffectInstruction(name,e) -> name ^ " := " ^ (prettyprint_expr e)
    | IfInstruction(e,b) -> "IF " ^ (prettyprint_expr e) ^ " THEN \n" ^ (prettyprint_bloc b)
    | IfElseInstruction(e,b1,b2) -> "IF " ^ (prettyprint_expr e) ^ " THEN :\n" ^ (prettyprint_bloc b1) ^ " \n ELSE :\n" ^ (prettyprint_bloc b2)
    | DeclInstruction (t, e) -> let s = type_string t in s ^ prettyprint_decl_list e ^ "\n"

and type_string t =
  match t with
  | Type_Int -> "INT "

and prettyprint_decl_list e = List.fold_left (fun x y -> x ^ y) "" e
    
and prettyprint_bloc c =
  match c with
  | t::q, l2 -> prettyprint_instr t ^ prettyprint_bloc (q,l2)
  | [], t::q -> prettyprint t ^ prettyprint_bloc ([],q)
  | [], [] -> " "

and prettyprint ast =
  match ast with
  | Expr(e) ->  prettyprint_expr e
  | Instr(inst) ->  prettyprint_instr inst ^ "\n"
  | Bloc(c) -> "{\n" ^ prettyprint_bloc c ^ "\n }"

(* TODO : extend when you extend the language *)
