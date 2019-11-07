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
<<<<<<< HEAD
  | DeclInstruction of typ * string
  | IfInstruction of expression * bloc
  | IfElseInstruction of expression * bloc * bloc
=======
  | DeclInstruction of typ * (string list)
                         
type codeObj = Expr of expression | Instr of instruction | Bloc of (codeObj list)
>>>>>>> 590edf970caea3f367059dc3f69cc2017cc147f1

and codeObj = Expr of expression | Instr of instruction | Bloc of (codeObj list)

and bloc = codeObj list

type program = codeObj list
