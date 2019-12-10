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
  | Func (id, args) -> id ^ "(" ^ (prettyprint_args args) ^ ")"

and prettyprint_instr i =
    match i with
    | AffectInstruction(name,e) -> (prettyprint_var name) ^ " := " ^ (prettyprint_expr e)
    | IfElseInstruction(e,b1,b2) -> "IF " ^ (prettyprint_expr e) ^ " THEN \n" ^ "{\n" ^ (prettyprint_bloc b1) ^ "} \nELSE \n" ^ "{\n" ^ (prettyprint_bloc b2) ^ "}\nFI"
    | DeclInstruction (t, e) -> let s = type_string t in s ^ prettyprint_var_list e ^ "\n"
    | WhileInstruction (e, b) -> "WHILE " ^ (prettyprint_expr e) ^ "\n" ^
                                 "DO\n" ^ "{\n" ^ prettyprint_bloc b ^ "}\n" ^ "DONE"
    | ReturnInstruction e -> "RETURN " ^ prettyprint_expr e
    | ProtoInstruction (id, t, args) -> "PROTO " ^ type_string t ^ id ^ "(" ^
                                        prettyprint_var_list args ^ ")\n"
    | PrintInstruction s -> "PRINT " ^ prettyprint_printable_list s
    | ReadInstruction v -> "READ " ^ prettyprint_var_list v
    | CallInstruction (id, args) -> id ^ "(" ^ prettyprint_args args

and prettyprint_printable p =
  match p with
  | P_str s -> "\"" ^ s ^ "\""
  | P_expr e -> prettyprint_expr e

and prettyprint_printable_list s = List.fold_left (fun x y -> x ^ y) "" (add_comma (List.map prettyprint_printable s))

and type_string t =
  match t with
  | Type_Int -> "INT "
  | Type_void -> "VOID "
  | Type_tab _ -> "INT "

and prettyprint_var_list e = List.fold_left (fun x y -> x ^ y) "" (add_comma (List.map prettyprint_var e))

and prettyprint_args e = List.fold_left (fun x y -> x ^ y) "" (add_comma (List.map prettyprint_expr e))
    
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
  | Function(id, t, args, b) -> "FUNC " ^ (type_string t) ^ id ^ "(" ^ (prettyprint_var_list args) ^ ")\n" ^ prettyprint (Bloc b)

(* TODO : extend when you extend the language *)
