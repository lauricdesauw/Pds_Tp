(* ASD type *)
type obj = string;;
type cpl = Obj of obj |Txt of string ;; 
type attr = Attr of obj * (cpl list);;
type ens = Ens of obj * (attr list);;
type document = Doc of (ens list);;

let read_obj o = "<"^o^">";;

let read_cpl c = 
    match c with 
    | Txt(t) -> "\""^t^"\" "
    | Obj(p) -> read_obj(p);;

let read_attr name attr = 
    match attr.var with 
    | []   -> ""
    | t::q -> read_obj(name)^read_obj(attr.name)^read_cpl(t);;

let rec read_ens ens = 
    match ens.attr with 
    | []    -> ""
    | t::q  -> let ens_red = {name = ens.name; attr = q} in read_attr(ens.name,t)^read_ens(ens_red);;


let rec read_doc doc = 
    match doc with 
    | []   -> ""
    | p::q -> read_ens(p)^read_doc(q);;

(* Function to generate the document out of the AST *)
let rec ntriples_of_ast ast = "…"
  (* Fill here! *)
