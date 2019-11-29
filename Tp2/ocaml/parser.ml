open ASD
open Token

type wrapper =
  | Wrap_None
  | Wrap_Expression of expression
  | Wrap_EList of expression list

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
                      | [< 'RP >] -> e1
                      | [< >] -> e1

  (* TODO : that's all? *)

and factor = parser
           | [< e1 = primary; e = factor_aux e1 >] -> e
           | [< 'LP ; e = expression >] -> e


and factor_aux e1 = parser
                  | [< 'MUL ; e2 = primary ; e = factor_aux (MulExpression (e1, e2)) >] -> e
                  | [< 'DIV ; e2 = primary ; e = factor_aux (DivExpression (e1, e2)) >] -> e
                  | [<>] -> e1
(* TODO : that's all? *)

and primary = parser
            | [< 'INTEGER x >] -> IntegerExpression x
            | [< v = variable >] -> VarExpression v
            | [< 'LP ; e = expression >] -> e
  (* TODO : that's all? *)

and variable = parser
             | [< 'IDENT id; e = tab_or_func >] -> to_variables id e

and tab = parser
        | [< 'LSQ; e = expression; 'RSQ >] -> Wrap_Expression e
        | [< 'LP; e = expression; q = arguments; 'RP >] -> Wrap_EList (e::q)
        | [< >] -> Wrap_None

and to_variables id = function
  | Wrap_None -> Var id
  | Wrap_Expression e -> Tab (id, e)
  | Wrap_EList args -> Func (id, args)

and arguments = parser
              | [< 'COM; e = expression; q = arguments >] -> e::q
              | [< >] -> []

and comma = parser
  | [< 'COM >] -> ()

and instruction = parser
                | [< v = variable; 'ASSIGN; e = expression; >] -> Instr(AffectInstruction(v,e))
                | [<'IF_KW; cond = expression; 'THEN_KW; b_if = if_while_bloc; b_else = elsebloc; 'FI_KW>]
                  -> Instr(IfElseInstruction(cond,b_if,b_else))
                | [< 'INT_KW; id = variable; id_list = decl >] -> Instr (DeclInstruction(Type_Int, id::id_list))
                | [< 'WHILE_KW; cond = expression; 'DO_KW; b = if_while_bloc; 'DONE_KW >]
                  -> Instr(WhileInstruction(cond,b))

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
