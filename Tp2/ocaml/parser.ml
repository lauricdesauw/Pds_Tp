open ASD
open Token
open String

type wrapper =
  | Wrap_None
  | Wrap_Expression of expression
  | Wrap_EList of expression list
  | Wrap_String of string

(* p? *)
let opt p = parser
  | [< x = p >] -> Some x
  | [<>] -> None

(* p* *)
let rec many p = parser
  | [< x = p; l = many p >] -> x :: l
  | [<>] -> []

(* p+ *)
let some p = parser
  | [< x = p; l = many p >] -> x :: l

(* p (sep p)* *)
let rec list1 p sep = parser
  | [< x = p; l = list1_aux p sep >] -> x :: l
and list1_aux p sep = parser
  | [< _ = sep; l = list1 p sep >] -> l
  | [<>] -> []

(* (p (sep p)* )? *)
let list0 p sep = parser
  | [< l = list1 p sep >] -> l
  | [<>] -> []


(* TODO : change when you extend the language *)
let rec program = parser
                | [< e = bloc; _ = Stream.empty ?? "unexpected input at the end" >] -> Bloc e

and expression = parser
                   | [< e1 = factor; e = expression_aux e1 >] -> e

and expression_aux e1 = parser
                      | [< 'PLUS ; e2 = factor ; e = expression_aux (AddExpression (e1, e2)) >] -> e
                      | [< 'MINUS ; e2 = factor ; e = expression_aux (SubExpression (e1, e2)) >] -> e
                      | [< 'MUL ; e2 = factor ; e = expression_aux (MulExpression (e1, e2)) >] -> e
                      | [< 'DIV ; e2 = factor ; e = expression_aux (DivExpression (e1, e2)) >] -> e
                      | [< >] -> e1

  (* TODO : that's all? *)

and factor = parser
           | [< e1 = primary; e = factor_aux e1 >] -> e
           | [< 'LP ; e = expression; 'RP >] -> e


and factor_aux e1 = parser
                  | [< 'MUL ; e2 = primary ; e = factor_aux (MulExpression (e1, e2)) >] -> e
                  | [< 'DIV ; e2 = primary ; e = factor_aux (DivExpression (e1, e2)) >] -> e
                  | [<>] -> e1
(* TODO : that's all? *)

and primary = parser
            | [< 'INTEGER x >] -> IntegerExpression x
            | [< v = variable >] -> VarExpression v
            | [< 'LP ; e = expression; 'RP >] -> e
  (* TODO : that's all? *)

and variable = parser
             | [< 'IDENT id; e = tab_or_func_or_field >] -> to_variables id e

and tab_or_func_or_field = parser
                         | [< 'LSQ; e = expression; 'RSQ >] -> Wrap_Expression e
                         | [< 'LP; q = arguments; 'RP >] -> Wrap_EList q
                         | [< 'DOT; 'IDENT field >] -> Wrap_String field
                         | [< >] -> Wrap_None

and to_variables id = function
  | Wrap_None -> Var id
  | Wrap_Expression e -> Tab (id, e)
  | Wrap_EList args -> Func (id, args)
  | Wrap_String field -> Field (id, field)

and arguments = parser
              | [< e = expression; q = arguments_aux >] -> e::q
              | [< >] -> []

and arguments_aux = parser
              | [< 'COM; e = expression; q = arguments_aux >] -> e::q
              | [< >] -> []

and comma = parser
  | [< 'COM >] -> ()

and assign_or_call v = parser
                     | [< 'ASSIGN; e = expression >] -> AffectInstruction (v, e)
                     | [< >] -> CallInstruction v
                     
and instruction = parser
                | [< v = variable; i = assign_or_call v >] -> Instr i
                | [<'IF_KW; cond = expression; 'THEN_KW; b_if = if_while_bloc; b_else = elsebloc; 'FI_KW>]
                  -> Instr(IfElseInstruction(cond,b_if,b_else))
                | [< t = typ; id = variable; id_list = decl >] -> Instr (DeclInstruction(t, id::id_list))
                | [< 'WHILE_KW; cond = expression; 'DO_KW; b = if_while_bloc; 'DONE_KW >]
                  -> Instr(WhileInstruction(cond,b))
                | [< 'PROTO_KW; t = typ; 'IDENT f; 'LP; q = proto_var; 'RP >] -> Instr (ProtoInstruction (f, t, q))
                | [< 'FUNC_KW; t = typ; 'IDENT f; 'LP; q = proto_var; 'RP; b = if_while_bloc >]
                  -> Function (f, t, q, b)
                | [< 'RETURN_KW; e = expression >] -> Instr (ReturnInstruction e)
                | [< 'PRINT_KW; s = printables >] -> Instr (PrintInstruction s)
                | [< 'READ_KW; v = variables >] -> Instr (ReadInstruction v)
                | [< 'STRUCT_KW; 'IDENT s; 'LB; b = bloc; 'RB >] ->
                  let decl, _ = b in Struct (s, decl)

and variables = parser
              | [< v = variable; q = variables_aux >] -> v::q

and variables_aux = parser
                  | [< 'COM; v = variable; q = variables_aux >] -> v::q
                  | [< >] -> []

and printables = parser
               | [< p = printable; q = printables_aux >] -> p::q

and printable = parser
              | [< 'TEXT t >] -> P_str (implode (make_linebreaks (explode t)))
              | [< e = expression >] -> P_expr e

and explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

and implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  Bytes.to_string (imp 0 l)

and make_linebreaks = function
    | [] -> []
    | [t] -> [t]
    | t1::t2::q when t1 = Char.chr 92 && t2 = 'n' -> (Char.chr 10)::(make_linebreaks q)
    | t1::t2::q -> t1::(make_linebreaks (t2::q))

and printables_aux = parser
                   | [< 'COM; p = printable; q = printables_aux >] -> p::q
                   | [< >] -> []

and typ = parser
        | [< 'INT_KW >] -> Type_Int
        | [< 'VOID_KW >] -> Type_void
        | [< 'STRUCT_KW; 'IDENT s >] -> Type_struct s

and proto_var = parser
              | [< a = variable; q = proto_var_aux >] -> a::q
              | [< >] -> []

and proto_var_aux = parser
                  | [< 'COM; a = variable; q = proto_var_aux >] -> a::q
                  | [< >] -> []
                    
and if_while_bloc = parser
    | [<i = instruction>] -> ([],[i] : bloc)
    | [<'LB; b = bloc; 'RB >] -> b

and elsebloc = parser
    | [< 'ELSE_KW; b = if_while_bloc >] -> b
    | [<>] -> ([],[] : bloc)

and decl = parser
         | [< 'COM; v = variable; tl = decl >] -> v::tl
         | [< >] -> []

and bloc = parser
| [< c = bloc_aux >] -> split_bloc c

and split_bloc = function
  | [] -> ([] : instruction list), ([] : codeObj list)
  | (Instr(DeclInstruction (p,l)))::q -> let decl, aff = split_bloc q in
    ((DeclInstruction (p,l))::decl), aff
  | i::q -> [], (i::q)

and bloc_aux = parser
| [< i = instruction ; q = bloc_aux >] -> i::q
| [< 'LB ; c1 = bloc ; 'RB ; c2 = bloc_aux >] -> (Bloc c1)::c2
| [< >] -> []
