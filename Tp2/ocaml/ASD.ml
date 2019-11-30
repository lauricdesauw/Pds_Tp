(* TODO : extend when you extend the language *)

type ident = string

type typ =
  | Type_Int
  | Type_void
  | Type_tab of int
    
type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression
  | IntegerExpression of int
  | VarExpression of variables
  | FuncExpression of string * (expression list)

and variables =
  | Var of string
  | Tab of string * expression

type instruction =
  | AffectInstruction of variables * expression
  | DeclInstruction of typ * (variables list)
  | IfElseInstruction of expression * bloc * bloc
  | WhileInstruction of expression * bloc
  | ReturnInstruction of expression
  | ProtoInstruction of string * typ * (variables list)

and codeObj = Expr of expression | Instr of instruction | Bloc of bloc | Function of string * typ * (variables list) * bloc

and bloc = (instruction list) * (codeObj list)

type program = codeObj
