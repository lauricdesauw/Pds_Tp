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
                         
type codeObj = Expr of expression | Instr of instruction | Bloc of (codeObj list)

type bloc = codeObj list 

type program = codeObj list
