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
  | VarExpression of variables

and variables =
  | Var of string
  | Tab of string * expression

type instruction =
  | AffectInstruction of variables * expression
  | DeclInstruction of typ * (variables list)
  | IfElseInstruction of expression * bloc * bloc
  | WhileInstruction of expression * bloc

and codeObj = Expr of expression | Instr of instruction | Bloc of bloc

and bloc = (instruction list) * (codeObj list)

type program = codeObj
