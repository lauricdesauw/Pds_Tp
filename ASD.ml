(* ASD type *)
type obj = string;;
type cpl = Obj of obj |Txt of string ;;
type attr = Attr of obj * (cpl list);;
type ens = Ens of obj * (attr list);;
type document = Doc of (ens list);;

let read_obj o = "<"^o^"> ";;

let read_cpl c =
    match c with
    | Txt(t) -> "\""^t^"\" "
    | Obj(p) -> read_obj(p);;

let rec read_attr attr =
    match attr with
    | Attr (a,l)   -> List.map (function c -> (read_obj a) ^ (read_cpl c)) l
;;

let rec read_ens ens =
    match ens with
    | Ens (a,l) -> let l1 = (List.map (function c ->  (read_attr c)) l) in
             let reduce = function c -> (List.fold_left (fun x y ->  x ^ (read_obj a) ^ y ^ ".\n ") "" c) in
             let l2 = List.map reduce l1 in
             List.fold_left (fun x y -> x ^ y) "" l2
;;

let rec ntriples_of_ast doc =
    match doc with
    | Doc []   -> ""
    | Doc (p::q) -> read_ens(p)^ntriples_of_ast(Doc q)
;;

(* Function to generate the document out of the AST*)
(*
let suj1 = "Moi";;
let suj2 = "Elle";;
let verb1 = "Aimer";;
let verb2 = "Manger";;
let obj1 = Obj(suj1);;
let obj2 = Obj("banane");;
let obj3 = Txt("PDS");;
let obj4 = Obj(suj2);;

let attr1 = Attr (verb1,obj4::obj3::[]);;
let attr2 = Attr (verb1,obj1::[]);;
let attr3 = Attr (verb2,obj2::[]);;

let ens1 = Ens (suj1,attr1::[]);;
let ens2 = Ens (suj2,attr2::attr3::[]);;

let doc = Doc (ens1::ens2::[]);;
print_string (ntriples_of_ast doc);;
*)
