(* TODO : extend when you extend the language *)

type ident = string

type typ =
  | Type_Int

type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression
  | IntegerExpression of int
  | VarExpression of string

type instruction =
  | AffectInstruction of string * expression
  | DeclInstruction of typ * (string list)
  | IfInstruction of expression * bloc
  | IfElseInstruction of expression * bloc * bloc

and codeObj = Expr of expression | Instr of instruction | Bloc of bloc

and bloc = (instruction list) * (codeObj list)

type program = codeObj list
