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
    | VarExpression v -> prettyprint_var v

and prettyprint_var v =
  match v with
  | Var id -> id
  | Tab (id, e) -> id ^ "[" ^ (prettyprint_expr e) ^ "]"

and prettyprint_instr i =
    match i with
    | AffectInstruction(name,e) -> (prettyprint_var name) ^ " := " ^ (prettyprint_expr e)
    | IfElseInstruction(e,b1,b2) -> "IF " ^ (prettyprint_expr e) ^ " THEN \n" ^ "{\n" ^ (prettyprint_bloc b1) ^ "} \nELSE \n" ^ "{\n" ^ (prettyprint_bloc b2) ^ "}\nFI\n"
    | DeclInstruction (t, e) -> let s = type_string t in s ^ prettyprint_decl_list e ^ "\n"
    | WhileInstruction (e, b) -> "WHILE " ^ (prettyprint_expr e) ^ "\n" ^
                                 "DO\n" ^ "{\n" ^ prettyprint_bloc b ^ "}\n" ^ "DONE"

and type_string t =
  match t with
  | Type_Int -> "INT "

and prettyprint_decl_list e = List.fold_left (fun x y -> x ^ y) "" (add_comma (List.map prettyprint_var e))

and add_comma l =
  match l with
  | [] -> []
  | [t] -> [t]
  | t::q -> (t ^ ", ")::(add_comma q)
    
    
and prettyprint_bloc c =
  match c with
  | t::q, l2 -> prettyprint_instr t ^ prettyprint_bloc (q,l2)
  | [], t::q -> prettyprint t ^ prettyprint_bloc ([],q)
  | [], [] -> ""

and prettyprint ast =
  match ast with
  | Expr(e) ->  prettyprint_expr e
  | Instr(inst) ->  prettyprint_instr inst ^ "\n"
  | Bloc(c) -> "{\n" ^ prettyprint_bloc c ^ "}\n"

(* TODO : extend when you extend the language *)
