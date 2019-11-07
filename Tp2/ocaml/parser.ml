open ASD
open Token

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
                | [< e = bloc; _ = Stream.empty ?? "unexpected input at the end" >] -> e

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
            | [< 'IDENT id >] -> VarExpression id
            | [< 'LP ; e = expression >] -> e
  (* TODO : that's all? *)

and comma = parser
  | [< 'COM >] -> ()

and instruction = parser
                | [<'IDENT id; 'ASSIGN; e = expression; >] -> Instr(AffectInstruction(id,e))
                | [<'IF_KW; e = expression; 'THEN_KW; b1 = bloc; 'ELSE_KW; b2 = bloc>] -> Instr(IfElseInstruction(e,b1,b2))
                | [<'IF_KW; e = expression; 'THEN_KW; b = bloc>] -> Instr(IfInstruction(e,b))
                | [< 'INT_KW; id_list = decl >] -> DeclInstruction(Type_Int, id_list)

and decl = parser
         | [< 'IDENT id ; tl = decl >] -> id::tl
         | [< >] -> []

and bloc = parser
| [< c = bloc_aux >] -> Bloc (split_bloc c)

and split_bloc = function
  | [] -> [], []
  | (DeclInstruction p)::q -> let decl, aff = split_bloc q in
    ((DeclInstruction p)::decl), aff
  | i::q -> [], (i::q)

and bloc_aux = parser
| [< i = instruction ; q = bloc_aux >] -> i::q
| [< 'LB ; c1 = bloc ; 'RB ; c2 = bloc_aux >] -> c1::c2
| [< >] -> []
