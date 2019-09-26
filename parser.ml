open ASD
open Token

(* This gives some stream parser usage examples. You can discard them once you understood the syntax *)
(* These are high-level generic functions: they take a parser as argument *)
(* p? *)
let opt p = parser
  | [< x = p >] -> Some x
  | [<>] -> None

(* p* *)
let rec many p = parser
  | [< x = p; l = many p >] -> x :: l
  | [<>] -> []

(* p+ *)
let some p = parser
  | [< x = p; l = many p >] -> x :: l

(* Simple example usage: this function count the number of `SEMICOLON' *)
(* Its takes a Stream of token as argument, and gives an integer *)
let parse_semicolon : token Stream.t -> int = parser
  | [< 'SEMICOLON >] -> 1 (* Parsers need to match a `SEMICOLON' token *)

let count_semicolon = parser
  | [< l = many parse_semicolon >] -> List.length l (* Parser needs to match `many' `SEMICOLON' and returns the count *)



(* let rec parse = parser *)
(*  Fill here! *);
