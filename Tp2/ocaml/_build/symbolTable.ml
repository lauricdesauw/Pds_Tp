open List
open ASD

(* This file contains the symbol table definition. *)
(* A symbol table contains a set of ident and the  *)
(* corresponding symbols.                          *)
(* The order is important: this first match count  *)

type function_symbol_state = Defined | Declared

type function_symbol = {
  return_type: typ;
  identifier: ident;
  arguments: symbol_table;
  state: function_symbol_state;
}

and symbol =
  | VariableSymbol of typ * ident
  | FunctionSymbol of function_symbol

and symbol_table = symbol list


(* public interface *)
let lookup tab id =
  let rec assoc key = function (* like List.assoc, but with deep hidden keys *)
    | ((VariableSymbol (_, id)) as r) :: q
    | (FunctionSymbol {identifier = id; _} as r) :: q ->
        if key = id then
          Some r
        else
          assoc key q
    | [] -> None
  in assoc id tab

let add tab sym = sym :: tab

(* Note : obviously not symmetric *)
let merge = (@)
