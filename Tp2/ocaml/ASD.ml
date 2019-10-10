(* TODO : extend when you extend the language *)

type ident = string


type instruction = 
    | AffectInstruction of string * expression
    
type expression =
  | AddExpression of expression * expression
  | SubExpression of expression * expression
  | MulExpression of expression * expression
  | DivExpression of expression * expression
  | IntegerExpression of int

type codeObj = Expr of expression | Instr of instruction

type typ =
  | Type_Int

type program = expression
