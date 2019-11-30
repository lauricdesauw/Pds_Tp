open List
open ASD
open Utils
   
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
                
let rec add_list typ sym_l  tab =
  match sym_l with
  | [] -> tab
  | sym::sym_l' -> match sym with
                   | Var(name) -> 
                      VariableSymbol(typ,name)::(add_list typ sym_l' tab)
                   |Tab(name,offset_expr) -> match offset_expr with
                                             | IntegerExpression(size) ->
                                                VariableSymbol((Type_tab(size)),name)::(add_list typ sym_l' tab)
                                             | _ -> raise Wrong_decl_expr
                                          
                     
let get_type sym =
  match sym with
  | VariableSymbol(t,name) -> t
  | FunctionSymbol(f) -> f.return_type
(* Note : obviously not symmetric *)
let merge = (@)








































































