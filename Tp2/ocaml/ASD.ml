(* TODO : extend when you extend the language *)

type ident = string

type expression =
  | AddExpression of expression * expression
  | IntegerExpression of int

type typ =
  | Type_Int

type program = expression
