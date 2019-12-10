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
  | VariableSymbol of typ * ident * string
  | FunctionSymbol of function_symbol

and symbol_table = symbol list


(* public interface *)
let lookup tab id =
  let rec assoc key = function (* like List.assoc, but with deep hidden keys *)
    | ((VariableSymbol (_, id,_)) as r) :: q
    | (FunctionSymbol {identifier = id; _} as r) :: q ->
        if key = id then
          Some r
        else
          assoc key q
    | [] -> None
  in assoc id tab

let add tab sym = sym :: tab
                
let rec add_list typ sym_l  tab id_l=
  match sym_l, id_l with
  | [],_ -> tab
  | sym::sym_l', id::id_l' -> match sym with
                   | Var(name) -> 
                      VariableSymbol(typ,name,id)::(add_list typ sym_l' tab id_l')
                   |Tab(name,offset_expr) -> match offset_expr with
                                             | IntegerExpression(size) ->
                                                VariableSymbol((Type_tab(size)),name,id)::(add_list typ sym_l' tab id_l')
                                             | _ -> raise Wrong_decl_expr
                                          
                     
let get_type sym =
  match sym with
  | VariableSymbol(t,name,_) -> t
  | FunctionSymbol(f) -> f.return_type

(* Note : obviously not symmetric *)
let merge = (@)

let rec add_var_to_symT param symT id_l=
  match param, id_l with
  | [],_ -> symT
  | Var(name)::q, id::id_l' -> VariableSymbol(Type_Int, name,id) :: (add_var_to_symT q symT id_l')

let rec str_of_tab symT =
  match symT with
  | [] -> ""
  | [t] -> (match t with
           | VariableSymbol(typ,ident,id) -> ident
           | FunctionSymbol(f) -> f.identifier)
         
  | t::q -> match t with
            | VariableSymbol(typ,ident,id) -> ident ^ ", " ^ (str_of_tab q)
           | FunctionSymbol(f) -> f.identifier ^ ", " ^ (str_of_tab q)                                 
let get_id symb =
  match symb with
  | VariableSymbol(_,_,id) -> id
